{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Lib
import Configuration.Dotenv (loadFile, defaultConfig)
import Control.Monad (when)
import Data.Text (isPrefixOf, toLower, Text)
import qualified Data.Text.IO as TIO

import UnliftIO (liftIO)
import UnliftIO.Concurrent 

import Discord
import Discord.Types
import qualified Discord.Requests as R

pingpongExample :: IO()
pingpongExample = do userFacingError <- runDiscord $ def
                                            { discordToken = "Bot <TOKEN>"
                                            , discordOnEvent = eventHandler }
                     TIO.putStrLn userFacingError

eventHandler :: Event -> DiscordHandler()
eventHandler event = case event of
    MessageCreate m -> if isPing (messageText m) then do
        _ <- restCall (R.CreateReaction (messageChannel m, messageId m) "thumbsdown")
        _ <- restCall (R.CreateMessage (messageChannel m) "Pong!")
        pure()
        else if isAbout (messageText m) then do
            _ <- restCall (R.CreateMessageEmbed (messageChannel m) "Henlo" $
             def { createEmbedAuthorName = "Hanako from Jibaku Shōnen Hanako-kun"
                 , createEmbedDescription = "Hanako was inspired by the anime/manga Jibaku Shōnen Hanako-kun (a.k.a. Toilet-Bound Hanako-kun).\n\nHanako version 0.1 was made and developed by:\n**Tetsuki Syu#1250**"
                 , createEmbedFooterText = "Hanako Bot: Release 0.1 | 2021-02-21"
                 , createEmbedAuthorIcon = Just $ CreateEmbedImageUrl "https://cdn.discordapp.com/avatars/727418737308729376/ad28ef3be1b56cee215a9dfbe3f516c7.webp?size=1024"
                 , createEmbedThumbnail = Just $ CreateEmbedImageUrl "https://cdn.discordapp.com/emojis/310831928360304653.png"
                 })
            pure()
        else
            pure()
    _ -> pure()

fromBot :: Message -> Bool
fromBot m = userIsBot (messageAuthor m)

isPing :: Text -> Bool
isPing = ("h?ping" `isPrefixOf`) . toLower

isAbout :: Text -> Bool
isAbout = ("h?about" `isPrefixOf`) . toLower

main :: IO ()
main = pingpongExample
