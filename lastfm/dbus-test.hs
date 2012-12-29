-- DBus in Haskell! Well I never!

{-# LANGUAGE OverloadedStrings #-}

-- https://john-millikin.com/software/haskell-dbus/reference/haskell-dbus/
import DBus
import DBus.Client

main = do
    client <- connectSession

    --reply <- call_ client
    --    (methodCall "/org/mpris/MediaPlayer2"
    --                "org.mpris.MediaPlayer2.Player"
    --                "Pause") {
    --        methodCallDestination = Just "org.mpris.MediaPlayer2.spotify"
    --    }

    reply <- call_ client
        (methodCall "/org/mpris/MediaPlayer2"
                    "org.freedesktop.DBus.Properties"
                    "Get") {
            methodCallDestination = Just "org.mpris.MediaPlayer2.spotify",
            methodCallBody = [
                toVariant ("org.mpris.MediaPlayer2.Player" :: String),
                toVariant ("Metadata" :: String)
            ]
        }
    print reply
