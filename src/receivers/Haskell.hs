{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Haskell (
    receivers
) where

import           Data.Aeson              ( FromJSON
                                         , eitherDecode
                                         , parseJSON
                                         , withObject
                                         , (.:) )
import           Data.Maybe              ( fromMaybe )
import           Data.Text.Encoding      ( encodeUtf8 )
import qualified Data.Text as T

import           Command
import           Discord                 ( DiscordHandler )
import           Discord.Types           ( Message )
import           GHC.Generics
import           Network.HTTP.Simple     ( httpLBS
                                         , setRequestQueryString
                                         , parseRequest
                                         , getResponseBody
                                         )
import           Pointfree               ( pointfree' )
import           UnliftIO                ( liftIO )

import           Command

receivers :: [Message -> DiscordHandler ()]
receivers =
    [ runCommand pointfree
    , runCommand doc
    , runCommand hoogle
    ]

-- | Maximum number of items to return from a Hoogle search
maxHoogleItems :: Int
maxHoogleItems = 5

hoogleURL :: Int -> String
hoogleURL n = "https://hoogle.haskell.org?mode=json&format=text&start=1&count="
                  <> show n <> "&hoogle="

data Repo = Repo
    { repoUrl  :: String
    , name     :: String
    } deriving (Show, Generic)
instance FromJSON Repo where
    parseJSON = withObject "Repo" $ \v -> Repo
        <$> v .: "url"
        <*> v .: "name"

data HoogleResp = HoogleResp
    { url    :: String
    , mdl    :: Repo
    , pkg    :: Repo
    , item   :: String
    , t      :: String
    , docs   :: String
    } deriving (Show, Generic)
instance FromJSON HoogleResp where
    parseJSON = withObject "HoogleResp" $ \v -> HoogleResp
        <$> v .: "url"
        <*> v .: "module"
        <*> v .: "package"
        <*> v .: "item"
        <*> v .: "type"
        <*> v .: "docs"

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
pointfree = command "pf" $ \m (Remaining code) ->
    respond m $ pf code
    where pf = T.pack . inlineCode . fromMaybe "" . pointfree' . T.unpack
          -- TODO: Strip double back-ticks to allow nicely formatted input


-- | Given a function name/type sig, this fetches the information from Hoogle.
getHoogle :: Int -> T.Text -> IO [HoogleResp]
getHoogle n name = do
    initReq <- parseRequest "https://hoogle.haskell.org"
    let req = setRequestQueryString
            [ ("mode", Just "json")
            , ("format", Just "text")
            , ("start", Just "1")
            , ("count", Just $ encodeUtf8 $ T.pack $ show n)
            , ("hoogle", Just $ encodeUtf8 name)
            ] initReq
    resp <- getResponseBody <$> httpLBS req
    return $ case eitherDecode resp of
         Left  e -> error $ "[WARN] Malformed Hoogle response: " <> e
         Right r -> r

-- | Pretty-prints the function name and its module
formatHoogleEntry :: HoogleResp -> T.Text
formatHoogleEntry r = T.pack $ inlineCode (item r) <> " from module "
                            <> inlineCode (name $ mdl r)

-- | Searches hoogle for matching entries
hoogle :: (MonadDiscord m, MonadIO m) => Command m
hoogle = command "hoogle" $ \m (Remaining name) -> do
    hdocs <- liftIO $ getHoogle maxHoogleItems name
    respond m $ T.intercalate "\n" $ map formatHoogleEntry hdocs


-- | Formats a 'HoogleResp' into a nice markdown representation
formatDoc :: HoogleResp -> T.Text
formatDoc r = formatHoogleEntry r <> "\n"
               <> T.pack (codeblock "hs" $ docs r)

-- | Gives the documentation for a given Haskell function (from online Hoogle)
-- >>> :doc map
doc :: (MonadDiscord m, MonadIO m) => Command m
doc = command "doc" $ \m (Remaining name) -> do
    hdoc <- liftIO $ getHoogle 1 name
    respond m $ formatDoc $ head hdoc
