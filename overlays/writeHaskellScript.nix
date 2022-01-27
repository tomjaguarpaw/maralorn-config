self: super:
let inherit (self) lib pkgs;
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
  writeHaskell = name:
    { libraries ? [ ], ghc ? pkgs.ghc, ghcArgs ? [ ], ghcEnv ? { } }:
    pkgs.writers.makeBinWriter
      {
        compileScript =
          let filename = lib.last (builtins.split "/" name);
          in
          ''
            cp $contentPath ${filename}.hs
            ${
              lib.concatStringsSep " "
              (lib.mapAttrsToList (key: val: ''${key}="${val}"'') ghcEnv)
            } ${ghc.withPackages (_: libraries)}/bin/ghc ${
              lib.escapeShellArgs ghcArgs
            } ${filename}.hs
            mv ${filename} $out
            ${pkgs.binutils-unwrapped}/bin/strip --strip-unneeded "$out"
          '';
      }
      name;

  # writeHaskellBin takes the same arguments as writeHaskell but outputs a directory (like writeScriptBin)
  writeHaskellBin = name: pkgs.writeHaskell "/bin/${name}";
  writeHaskellScript = { name ? "haskell-script", bins ? [ ], imports ? [ ] }:
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
      } ''
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

      import Shh
      import Relude
      import Say
      import qualified Relude.Unsafe as Unsafe
      import qualified Data.ByteString.Lazy as LBS
      import qualified Data.ByteString as BS
      import qualified Data.Text as Text
      import System.Environment (getArgs, setEnv)
      import Control.Exception (bracket, try)
      import Data.String.Interpolate (i)
      import Control.Concurrent.Async
      ${builtins.concatStringsSep "\n" (map (x: "import ${x}") imports)}

      -- Load binaries from Nix packages. The dependencies will be captured
      -- in the closure.
      loadFromBins (${
        self.haskellList
        (builtins.map toString (bins ++ [ self.coreutils self.nix ]))
      } :: [String])

      getNivPath :: Text -> Text -> IO Text
      getNivPath sources channel = do
        let expression = [i|(import #{sources}/nix/sources.nix)."#{channel}"|] :: String
        nix_build ["-Q", "-E", expression, "--no-out-link"] &> devNull
        escaped <- nix_instantiate ["--eval" :: String, "-E", [i|toString #{expression}|]] |> captureTrim
        pure . Text.dropAround ('"' ==) . decodeUtf8 . trim $ escaped

      aNixPath :: Text -> Text -> Text -> IO [String]
      aNixPath homeManagerChannel nixpkgsChannel path = concat <$> mapM getNivAssign
          [("home-manager", homeManagerChannel),
           ("nixpkgs", nixpkgsChannel),
           ("nixos-unstable", "nixos-unstable")]
        where
         tag name str = ["-I", [i|#{name :: Text}=#{str :: Text}|]] :: [String]
         getNivAssign (name, repo) = tag name <$> getNivPath path repo

      myNixPath :: Text -> IO [String]
      myNixPath = aNixPath "${self.home-manager-channel}" "${self.nixpkgs-channel}"

      buildSystemParams :: [String]
      buildSystemParams = ["<nixpkgs/nixos>", "-A", "system"]

      remoteBuildParams :: [String]
      remoteBuildParams = ["--builders", "@/etc/nix/machines"]

      main :: IO ()
      ${code}
    '';
  get-niv-path = self.writeHaskellScript { name = "get-niv-path"; } ''
    main = do
        [sources, channel] <- fmap toText <$> getArgs
        path <- getNivPath sources channel
        say path
  '';
}
