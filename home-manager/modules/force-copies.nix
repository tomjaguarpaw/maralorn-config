{ lib, config, ... }:
let
  paths = config.home.forceCopies.paths;
  disableCollisionCheck = path: ''
    if [[ -a $HOME/${path} ]]; then
      echo "Removing $HOME/${path}" because it is probably from a previous generation.
      rm -f $HOME/${path}
    fi
  '';
  copyPath = path: ''
    canonical=`readlink -f $newGenPath/home-files/${path}`
    if [[ ! -a $canonical ]]; then
      echo "File $newGenPath/home-files/${path} does not exist."
      exit 1
    fi
    if [[ ! -a $HOME/${path} ]]; then
      echo "There is no file at $HOME/${path} this is weird."
      exit 1
    fi
    echo "Overwriting $HOME/${path}"
    cp --remove-destination -T $canonical $HOME/${path};
  '';
in with lib;
{
  options.home.forceCopies.paths = mkOption {
        default = [];
        type = types.listOf types.str;
    };
  config.home.activation = {
    deleteForcedCopies = config.lib.dag.entryBefore ["checkLinkTargets"]
      (builtins.concatStringsSep "\n" (builtins.map disableCollisionCheck paths));
    forceCopies = config.lib.dag.entryAfter ["linkGeneration"]
      (builtins.concatStringsSep "\n" (builtins.map copyPath paths));
    };
}
