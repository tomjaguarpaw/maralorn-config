self: super: {
  haskellList = list: ''["${builtins.concatStringsSep ''", "'' list}"]'';
  writeHaskellScript = { name ? "haskell-script", bins ? [ ], imports ? [ ] }:
    code:
    self.writers.makeBinWriter {
      compileScript = ''
        cp $contentPath ${name}.hs
        ${self.scriptGhc}/bin/ghc ${name}.hs -threaded -Wall -Wno-unused-top-binds -Wno-missing-signatures -Wno-type-defaults -Wno-unused-imports -Werror
        mv ${name} $out
        ${self.binutils-unwrapped}/bin/strip --strip-unneeded "$out"
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
      {-# LANGUAGE PartialTypeSignatures #-}

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
      remoteBuildParams = ["--builders", "@/etc/nix/machines", "--max-jobs", "1"]

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
