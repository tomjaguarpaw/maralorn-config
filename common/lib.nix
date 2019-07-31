rec {
  pkgs = import <nixpkgs> { };
  unstable = import <unstable> { };
  sources = import ../nix/sources.nix;
  unBreak = pkg:
    unstable.haskell.lib.overrideCabal pkg (drv: {
      broken = false;
      doCheck = false;
    });
  shh = unBreak unstable.haskellPackages.shh;
  ghc = unstable.ghc.withPackages
    (p: [ (unBreak p.shh) p.brittany p.hlint p.ghcid ]);
  haskellList = list: ''["${builtins.concatStringsSep ''", "'' list}"]'';
  writeHaskellScript = { name ? "haskell-script", bins ? [ pkgs.coreutils ]
    , libraries ? [ ], imports ? [ ] }:
    code:
    unstable.writers.writeHaskellBin name {
      libraries = libraries
        ++ [ shh unstable.haskellPackages.string-interpolate ];
    } ''
      {-# LANGUAGE DeriveDataTypeable #-}
      {-# LANGUAGE TemplateHaskell #-}
      {-# LANGUAGE QuasiQuotes #-}

      import Shh
      import Data.String.Interpolate (i)
      import qualified Data.ByteString as BS
      import qualified Data.ByteString.Lazy as LBS
      import qualified Data.ByteString.Lazy.Char8 as LBSC
      import qualified Data.Text as T
      import qualified Data.Text.Lazy as LT
      import qualified Data.Text.Encoding as TE
      import qualified Data.Text.Lazy.Encoding as LTE
      import System.Environment (getArgs)
      ${builtins.concatStringsSep "\n" (map (x: "import ${x}") imports)}

      -- Load binaries from Nix packages. The dependencies will be captured
      -- in the closure.
      loadFromBins ${
        haskellList (builtins.map toString (bins ++ [ pkgs.coreutils ]))
      }

      ${code}
    '';
  get-niv-path = writeHaskellScript {
    name = "get-niv-path";
    bins = [ pkgs.nix ];
    imports = [ "System.Console.CmdArgs.Implicit" ];
    libraries =
      [ unstable.haskellPackages.cmdargs unstable.haskellPackages.text ];
  } ''

    trimQuotation = pureProc $ LTE.encodeUtf8 . LT.dropAround ('"' ==) . LTE.decodeUtf8 . trim

    main = do
          [sources, channel] <- getArgs
          let expr = [i|(import #{sources}).#{channel}|]
          nix_build ["-Q", "-E", expr, "--no-out-link"] &> devNull
          nix_instantiate ["--eval", "-E", [i|toString #{expr}|]] |> trimQuotation
  '';
  home-manager = pkgs.callPackage <home-manager/home-manager> { };
  gcRetentionDays = 5;
}
