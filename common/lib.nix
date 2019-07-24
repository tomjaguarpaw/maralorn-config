let
  pkgs = import <nixpkgs> {};
  unstable = import <unstable> {};
  shh = unstable.haskell.lib.overrideCabal unstable.haskellPackages.shh (drv: {
    broken = false;
    doCheck = false;
  });
  writeHaskellScript = { name ? "haskell-script", bins ? [pkgs.coreutils], libraries ? [], imports ? []}: code:
    unstable.writers.writeHaskellBin name { libraries = libraries ++ [shh]; } ''
      {-# LANGUAGE DeriveDataTypeable #-}
      {-# LANGUAGE TemplateHaskell #-}
      import Shh
      ${builtins.concatStringsSep "\n" (map (x: "import ${x}") imports)}

      -- Load binaries from Nix packages. The dependencies will be captured
      -- in the closure.
      loadFromBins ["${builtins.concatStringsSep ''", "'' (builtins.map toString bins)}"]

      ${code}
    '';
in {
    inherit writeHaskellScript;
    getNivPath = writeHaskellScript {
      name = "get-niv-path";
      bins = [pkgs.nix pkgs.coreutils];
      imports = ["System.Console.CmdArgs.Implicit" "Data.Text.Lazy" "Data.Text.Lazy.Encoding"];
      libraries = [ unstable.haskellPackages.cmdargs unstable.haskellPackages.text ];
    } ''

      data Path = Path {
        sources :: String,
        channel :: String
      } deriving (Show, Data, Typeable)

      path = Path{ sources = def &= argPos 0, channel = def &= argPos 1}

      main :: IO ()
      main = do
            path <- cmdArgs path
            path <- readTrim $ nix_instantiate "--eval" "-E" ("toString (import " ++ sources path ++ ")." ++ channel path)
            echo . unpack . dropAround ('"' ==) $ decodeUtf8 path
      '';
}
