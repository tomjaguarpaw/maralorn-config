final: _:
let
  inherit (final) lib pkgs;
in
{
  haskellList = list: ''["${builtins.concatStringsSep ''", "'' list}"]'';
  # writeHaskell takes a name, an attrset with libraries and haskell version (both optional)
  # and some haskell source code and returns an executable.
  #
  # Example:
  #   writeHaskell "missiles" { libraries = [ pkgs.haskellPackages.acme-missiles ]; } ''
  #     import Acme.Missiles
  #
  #     main = launchMissiles
  #   '';
  writeHaskell =
    name:
    {
      libraries ? [ ],
      ghc ? pkgs.ghc,
      ghcArgs ? [ ],
      ghcEnv ? { },
    }:
    pkgs.writers.makeBinWriter
      {
        compileScript =
          let
            filename = lib.last (builtins.split "/" name);
          in
          ''
            cp $contentPath ${filename}.hs
            ${lib.concatStringsSep " " (lib.mapAttrsToList (key: val: ''${key}="${val}"'') ghcEnv)} ${
              ghc.withPackages (_: libraries)
            }/bin/ghc ${lib.escapeShellArgs ghcArgs} ${filename}.hs
            mv ${filename} $out
            ${pkgs.binutils-unwrapped}/bin/strip --strip-unneeded "$out"
          '';
      }
      name;

  # writeHaskellBin takes the same arguments as writeHaskell but outputs a directory (like writeScriptBin)
  writeHaskellBin = name: pkgs.writeHaskell "/bin/${name}";
  writeHaskellScript =
    {
      name ? "haskell-script",
      bins ? [ ],
      imports ? [ ],
    }:
    code:
    pkgs.writeHaskellBin name
      {
        ghcArgs = [
          "-threaded"
          "-Wall"
          "-Wno-unused-top-binds"
          "-Wno-missing-signatures"
          "-Wno-type-defaults"
          "-Wno-unused-imports"
          "-Werror"
        ];
        libraries = builtins.attrValues pkgs.myHaskellScriptPackages;
      }
      ''
        {-# LANGUAGE DeriveDataTypeable #-}
        {-# LANGUAGE DeriveGeneric #-}
        {-# LANGUAGE DeriveAnyClass #-}
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
        {-# LANGUAGE PartialTypeSignatures #-}
        {-# LANGUAGE BlockArguments #-}
        {-# LANGUAGE ImportQualifiedPost #-}
        {-# LANGUAGE TypeApplications #-}

        import Shh
        import Relude
        import Say
        import Relude.Unsafe qualified as Unsafe
        import Data.ByteString.Lazy qualified as LBS
        import Data.ByteString.Lazy.Char8 qualified as LBSC
        import Data.ByteString qualified as BS
        import Data.ByteString.Char8 qualified as BSC
        import Data.Text qualified as Text
        import Data.Time qualified as Time
        import Data.List qualified as List
        import Data.String qualified as String
        import System.Environment (setEnv)
        import Control.Exception (bracket, try)
        import Data.String.Interpolate (i)
        import Control.Concurrent.Async
        ${builtins.concatStringsSep "\n" (map (x: "import ${x}") imports)}

        -- Load binaries from Nix packages. The dependencies will be captured
        -- in the closure.
        loadFromBins (${
          pkgs.haskellList (
            map lib.getBin (
              [
                pkgs.coreutils
                pkgs.nix
              ]
              ++ bins
            )
          )
        } :: [String])

        main :: IO ()
        ${code}
      '';
}
