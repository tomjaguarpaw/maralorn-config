rec {
  colors = {
    "foreground" = "#dddbff";
    "background" = "#000018";
    "black" = "#000000";
    "brightBlack" = "#55508f";
    "red" = "#e34b4f";
    "brightRed" = "#e34b4f";
    "green" = "#67b779";
    "brightGreen" = "#45b75e";
    "yellow" = "#ff9c00";
    "brightYellow" = "#ff9c00";
    "blue" = "#5c67ff";
    "brightBlue" = "#5c67ff";
    "magenta" = "#cb85ff";
    "brightMagenta" = "#cb85ff";
    "cyan" = "#17d0f4";
    "brightCyan" = "#17d0f4";
    "white" = "#dddbff";
    "brightWhite" = "#ffffff";
  };
  pkgs = import <nixpkgs> { };
  haskellList = list: ''["${builtins.concatStringsSep ''", "'' list}"]'';
  writeHaskellScript =
    { name ? "haskell-script", bins ? [ ], libraries ? [ ], imports ? [ ] }:
    code:
    pkgs.writers.makeBinWriter {
      compileScript = ''
        cp $contentPath ${name}.hs
        ${
          pkgs.ghc.withPackages (_:
            libraries ++ (builtins.attrValues {
              inherit (pkgs.haskellPackages)
                shh string-interpolate relude async say cmdargs text;
            }))
        }/bin/ghc ${name}.hs -threaded -Wall -Wno-unused-top-binds -Wno-missing-signatures -Wno-type-defaults -Wno-unused-imports -Werror
        mv ${name} $out
        ${pkgs.binutils-unwrapped}/bin/strip --strip-unneeded "$out"
      '';
    } "/bin/${name}" ''
      {-# LANGUAGE DeriveDataTypeable #-}
      {-# LANGUAGE TemplateHaskell #-}
      {-# LANGUAGE QuasiQuotes #-}
      {-# LANGUAGE OverloadedStrings #-}
      {-# LANGUAGE ExtendedDefaultRules #-}
      {-# LANGUAGE MultiWayIf #-}
      {-# LANGUAGE LambdaCase #-}
      {-# LANGUAGE ViewPatterns #-}
      {-# LANGUAGE ScopedTypeVariables #-}
      {-# LANGUAGE NoImplicitPrelude #-}
      {-# LANGUAGE TupleSections #-}

      import Shh
      import Relude
      import Say
      import qualified Relude.Unsafe as Unsafe
      import qualified Data.ByteString.Lazy as LBS
      import qualified Data.ByteString as BS
      import qualified Data.Text as Text
      import System.Environment (getArgs)
      import Control.Exception (bracket, try)
      import Data.String.Interpolate (i)
      import Control.Concurrent.Async
      ${builtins.concatStringsSep "\n" (map (x: "import ${x}") imports)}

      -- Load binaries from Nix packages. The dependencies will be captured
      -- in the closure.
      loadFromBins (${
        haskellList
        (builtins.map toString (bins ++ [ pkgs.coreutils pkgs.nix ]))
      } :: [String])

      getNivPath :: Text -> Text -> IO Text
      getNivPath sources channel = do
        let expression = [i|(import #{sources}/nix/sources.nix).#{channel}|] :: String
        nix_build ["-Q", "-E", expression, "--no-out-link"] &> devNull
        escaped <- nix_instantiate ["--eval" :: String, "-E", [i|toString #{expression}|]] |> captureTrim
        pure . Text.dropAround ('"' ==) . decodeUtf8 . trim $ escaped

      myNixPath :: Text -> IO [String]
      myNixPath path = (extraPaths ++) . concat <$> mapM getNivAssign ["home-manager", "nixpkgs", "unstable"]
        where
         tag name str = ["-I", [i|#{name :: Text}=#{str :: Text}|]] :: [String]
         getNivAssign name = tag name <$> getNivPath path name
         extraPaths = ["-I", [i|nixpkgs-overlays=#{path}/overlays|]]

      main :: IO ()
      ${code}
    '';
  get-niv-path = writeHaskellScript { name = "get-niv-path"; } ''
    main = do
        [sources, channel] <- fmap toText <$> getArgs
        path <- getNivPath sources channel
        say path
  '';
  home-manager = pkgs.callPackage <home-manager/home-manager> { };
}
