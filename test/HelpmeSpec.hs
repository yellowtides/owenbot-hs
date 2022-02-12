{-# LANGUAGE OverloadedStrings #-}
module HelpmeSpec ( spec ) where

import Discord.Types
import MonadDiscordMock
import Helpme
import Test.Hspec
import Test.HMock

spec :: Spec
spec = do
    describe ":helpme" $ do
        it "sends one message to the DMs" $ do
            example $ runMockT $ do
                let uid = read "123123123123123123"
                let dmid = read "456456456456456456"
                expect $ CreateDMMock uid
                    |-> ChannelDirectMessage { channelId = dmid }
                expect $ CreateMessageMock_ (eq dmid) anything
                    |-> Message { }

                sendHelpDM $ Message
                    { messageContent = ":helpme"
                    , messageAuthor = User { userId = uid }
                    }
        it "sends an owoified message" $ do
            example $ runMockT $ do
                let uid = read "123123123123123123"
                let dmid = read "456456456456456456"
                expect $ CreateDMMock uid
                    |-> ChannelDirectMessage { channelId = dmid }
                expect $ CreateMessageMock_ anything (endsWith "owo")
                    |-> Message {}

                sendHelpDM $ Message
                    { messageContent = ":helpme"
                    , messageAuthor = User { userId = uid }
                    }
