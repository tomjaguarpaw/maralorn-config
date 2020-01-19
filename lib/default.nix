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
  unstable = import <unstable> { };
  sources = import ../nix/sources.nix;
  unBreak = pkg:
    pkgs.haskell.lib.overrideCabal pkg (drv: {
      broken = false;
      doCheck = false;
    });
  shh = unBreak pkgs.haskellPackages.shh;
  ghc = pkgs.ghc.withPackages (p: [
    (unBreak p.shh)
    p.brittany
    p.hlint
    p.ghcid
    p.cabal-install
    p.classy-prelude
    p.haskell-ci
    p.shake
    p.hledger-lib
  ]);
  haskellList = list: ''["${builtins.concatStringsSep ''", "'' list}"]'';
  writeHaskellScript = { name ? "haskell-script", bins ? [ pkgs.coreutils ]
    , libraries ? [ ], imports ? [ ] }:
    code:
    pkgs.writers.writeHaskellBin name {
      libraries = libraries ++ [
        shh
        pkgs.haskellPackages.string-interpolate
        pkgs.haskellPackages.relude
      ];
    } ''
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

      import Shh
      import Relude
      import qualified Relude.Unsafe as Unsafe
      import qualified Data.ByteString as BS
      import qualified Data.Text as Text
      import System.Environment (getArgs)
      import Control.Exception (bracket)
      import Data.String.Interpolate (i)
      ${builtins.concatStringsSep "\n" (map (x: "import ${x}") imports)}

      -- Load binaries from Nix packages. The dependencies will be captured
      -- in the closure.
      loadFromBins (${
        haskellList (builtins.map toString (bins ++ [ pkgs.coreutils ]))
      } :: [String])

      ${code}
    '';
  get-niv-path = writeHaskellScript {
    name = "get-niv-path";
    bins = [ pkgs.nix ];
    imports = [ "System.Console.CmdArgs.Implicit" ];
    libraries = [ pkgs.haskellPackages.cmdargs pkgs.haskellPackages.text ];
  } ''

    trimQuotation = pureProc $ encodeUtf8 . Text.dropAround ('"' ==) . decodeUtf8 . trim

    main = do
          [sources, channel] <- getArgs
          let expr = [i|(import #{sources}).#{channel}|] :: String
          nix_build ["-Q", "-E", expr, "--no-out-link"] &> devNull
          nix_instantiate ["--eval" :: String, "-E", [i|toString #{expr}|]] |> trimQuotation
  '';
  home-manager = pkgs.callPackage <home-manager/home-manager> { };
  gcRetentionDays = 5;
}
