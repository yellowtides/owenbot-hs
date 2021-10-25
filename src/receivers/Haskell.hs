{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Haskell (commands) where

import Data.Aeson ((.:), FromJSON, Value(Object), eitherDecode, parseJSON, withObject)
import Data.List (isInfixOf)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)

import Command
import Data.Aeson.Types
import qualified Data.HashMap.Strict as HM
import Discord (DiscordHandler)
import Discord.Types (Message)
import GHC.Generics
import Network.HTTP.Simple
    ( getResponseBody
    , httpLBS
    , parseRequest
    , setRequestBodyURLEncoded
    , setRequestMethod
    , setRequestQueryString
    )
import Pointfree (pointfree')
import UnliftIO (liftIO)

commands :: [Command DiscordHandler]
commands = [pointfree, doc, hoogle, hoogType, eval]

-- | Maximum number of items to return from a Hoogle search
maxHoogleItems :: Int
maxHoogleItems = 5

hoogleURL :: Int -> String
hoogleURL n =
    "https://hoogle.haskell.org?mode=json&format=text&start=1&count="
        <> show n
        <> "&hoogle="

data Repo = Repo
    { repoUrl :: String
    , name    :: String
    }
    deriving (Show, Generic)
instance FromJSON Repo where
    parseJSON = withObject "Repo" $ \v -> Repo <$> v .: "url" <*> v .: "name"

data HoogleResp = HoogleResp
    { url  :: String
    , mdl  :: Repo
    , pkg  :: Repo
    , item :: String
    , t    :: String
    , docs :: String
    }
    deriving (Show, Generic)
instance FromJSON HoogleResp where
    parseJSON = withObject "HoogleResp" $ \v ->
        HoogleResp
            <$> v
            .:  "url"
            <*> v
            .:  "module"
            <*> v
            .:  "package"
            <*> v
            .:  "item"
            <*> v
            .:  "type"
            <*> v
            .:  "docs"

-- | Surrounds a String with back-ticks for nice formatting on Discord
inlineCode :: String -> String
inlineCode = ("``" ++) . (++ "``")

-- | Creates a large codeblock formatted by a given language code
codeblock :: String -> String -> String
codeblock lang = (("```" ++ lang ++ "\n") ++) . (++ "```")

-- | Converts given Haskell code to point-free form
-- /Note/: Don't escape any characters.
-- >>> :pf f x = 1 + x
-- f = (1 +)
--
-- >>> :pf inlineCode str = "`" ++ str ++ "`"
-- inlineCode = ("``" ++) . (++ "``")
pointfree :: (MonadDiscord m) => Command m
pointfree =
    help ("How would you write it point-free?\n" <> "Usage: `:pf <haskell expression>`")
        . command "pf"
        $ \m (Remaining code) -> respond m $ pf code
  where
    pf =
        T.pack
            . inlineCode
            . fromMaybe "Couldn't format this code!"
            . pointfree'
            . T.unpack
          -- TODO: Strip double back-ticks to allow nicely formatted input


-- | Given a function name/type sig, this fetches the information from Hoogle.
getHoogle :: Int -> T.Text -> IO [HoogleResp]
getHoogle n name = do
    initReq <- parseRequest "https://hoogle.haskell.org"
    let req = setRequestQueryString
            [ ("mode"  , Just "json")
            , ("format", Just "text")
            , ("start" , Just "1")
            , ("count" , Just $ encodeUtf8 $ T.pack $ show n)
            , ("hoogle", Just $ encodeUtf8 name)
            ]
            initReq
    resp <- getResponseBody <$> httpLBS req
    return $ case eitherDecode resp of
        Left  e -> error $ "[WARN] Malformed Hoogle response: " <> e
        Right r -> r

-- | Pretty-prints the function name and its module
formatHoogleEntry :: HoogleResp -> T.Text
formatHoogleEntry r =
    T.pack $ inlineCode (item r) <> " from module " <> inlineCode (name $ mdl r)

-- | Searches hoogle for matching entries
hoogle :: (MonadDiscord m, MonadIO m) => Command m
hoogle =
    help
            (  "See the top "
            <> T.pack (show maxHoogleItems)
            <> " results from hoogle.\n"
            <> "Usage: `:hoogle <query>`"
            )
        . command "hoogle"
        $ \m (Remaining name) -> do
            hdocs <- liftIO $ getHoogle maxHoogleItems name
            respond m $ T.intercalate "\n" $ map formatHoogleEntry hdocs


-- | Formats a 'HoogleResp' into a nice markdown representation
formatDoc :: HoogleResp -> T.Text
formatDoc r = formatHoogleEntry r <> "\n" <> T.pack (codeblock "hs" $ docs r)

-- | Gives the documentation for a given Haskell function (from online Hoogle)
-- >>> :doc map
doc :: (MonadDiscord m, MonadIO m) => Command m
doc =
    help
            (  "See the documentation for the Haskell function.\n"
            <> "Usage: `:doc <function>`"
            )
        . command "doc"
        $ \m (Remaining name) -> do
            hdoc <- liftIO $ head <$> getHoogle 1 name
            respond m $ formatDoc hdoc

-- | Gives the type of a given Haskell function (from online Hoogle, top entry)
-- >>> :type map
hoogType :: (MonadDiscord m, MonadIO m) => Command m
hoogType =
    help
            (  "See the type of the Haskell function.\n"
            <> "Usage: `:type <function>` or `:t <function>`"
            )
        . alias "t"
        . command "type"
        $ \m (Remaining name) -> do
            hdoc <- liftIO $ head <$> getHoogle 1 name
            respond m $ formatHoogleEntry hdoc

data TryHaskellResponse = TryHaskellSuccessResponse
    { thExpr :: String
    , thStdout :: [String]
    , thValue :: String
    , thType :: String
    }
    | TryHaskellErrorResponse
    { thError :: String
    } deriving Show

instance FromJSON TryHaskellResponse where
    parseJSON = withObject "TryHaskellResponse" $ \v -> case HM.lookup "success" v of
        Nothing -> do
            TryHaskellErrorResponse <$> v .: "error"
        Just _ -> do
            s <- v .: "success"
            TryHaskellSuccessResponse
                <$> s
                .:  "expr"
                <*> s
                .:  "stdout"
                <*> s
                .:  "value"
                <*> s
                .:  "type"

-- | Evaluates a Haskell expression using (Chris Done's) TryHaskell backend.
-- For more complexity (and loss of security), this can be implemented using
-- the GHC modules to parse the expression. However this can be dangerous and is
-- reinventing the wheel, so we entirely offload the concern to TryHaskell.
--
-- Uses https://haskellmooc.co.uk/ instead of https://tryhaskell.org/ since
-- it supports IO and looks like it's more updated.
-- >>> :eval 1 + 1
eval :: (MonadDiscord m, MonadIO m) => Command m
eval =
    help
            (  "Evaluate a Haskell expression.\n"
            <> "Usage: `:eval <expression>` or `:e <expression>`"
            )
        . alias "e"
        . command "eval"
        $ \m (Remaining expression) -> do
            initReq <- liftIO $ parseRequest "https://haskellmooc.co.uk/eval"
            let
                req = setRequestMethod "POST" $ setRequestBodyURLEncoded
                    [ ("exp", encodeUtf8 expression)
                    , ( "args"
                      , "[[],{\"/whatsthis.txt\":\"https://www.youtube.com/watch?v=DLzxrzFCyOs\",\"/admin/password.txt\":\"ceasar salad\"}]"
                      )
                    ]
                    initReq
            resp <- liftIO $ getResponseBody <$> httpLBS req

            let
                parsedADT = do
                    decodedValue  <- eitherDecode resp
                    decodedValue' <- case decodedValue of
                        (Object o) -> Right decodedValue
                        ds ->
                            Left
                                $  "TryHaskell didn't respond with an JSON Object: "
                                <> show ds
                    parseEither parseJSON decodedValue'
            case parsedADT of
                Left e -> do
                    respond m
                        $  "The command failed unexpectedly. "
                        <> "Try again later, or contact an OwenDev if it persists."
                    liftIO $ putStrLn $ "[WARN] " <> e
                Right r@TryHaskellSuccessResponse{} ->
                    respond m
                        $  T.pack
                        $  codeblock
                             "hs"
                             ("λ " <> thExpr r <> "\n" <> thValue r <> " :: " <> thType r
                             )
                        <> (if not (null $ thStdout r)
                             then "\nOutput:\n" <> codeblock "" (concat (thStdout r))
                             else ""
                           )
                Right r@TryHaskellErrorResponse{} ->
                    let
                        smartHelp
                            | "'\\8220'" `isInfixOf` (thError r)
                            = "Maybe: Try using \" characters instead of smart quotes (“”)."
                            | otherwise
                            = ""
                    in
                        respond m
                        $  T.pack
                        $  "!!! "
                        <> smartHelp
                        <> codeblock "hs" (thError r)
