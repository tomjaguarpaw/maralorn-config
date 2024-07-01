{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wall -Werror #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

import Data.String.Interpolate (i)
import Relude
import Shh
import System.Directory (getHomeDirectory)
import System.FilePath ((</>))

default (String)

-- Executables used.
load
  Absolute
  [ "hyprctl" -- NIX_BIN
  , "jq" -- NIX_BIN
  , "arbtt-import" -- NIX_BIN
  ]

sampleRateInSeconds :: Int
sampleRateInSeconds = 20

main :: IO ()
main = do
  home <- getHomeDirectory
  idle_state <- ignoreFailure (jq "-r" ".tag" (home </> ".idle_state")) |> captureTrim
  let inactive = case idle_state of
        "Idle" -> "1"
        _ -> "0"
  act <- hyprctl "activewindow" "-j" |> jq ".address" |> captureTrim
  hyprctl "clients" "-j"
    |> jq
      ( id @String
          [i|{date:now|strftime("%FT%TZ"),rate:#{sampleRateInSeconds * 1000},inactive:#{inactive},windows:map({title,program:.class,active:(.address == #{act})}),desktop:""}|]
      )
    |> arbtt_import "-a" "-t" "JSON"
