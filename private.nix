let
  privateExists = builtins.pathExists private/submodule-is-checked-out;
  explicitUsePrivate = builtins.getEnv "WITH_SECRETS" == "true";
  explicitNotUsePrivate = builtins.getEnv "WITH_SECRETS" == "false";
  usePrivate = !explicitNotUsePrivate && (explicitUsePrivate || privateExists);
  withSecrets = builtins.trace (if usePrivate then
    assert privateExists; "Building _with_ secrets!"
  else
    "Building _without_ secrets!") usePrivate;
in {
  inherit withSecrets;
  privatePath = name:
    let path = "/etc/nixos/private/${name}";
    in if withSecrets then assert builtins.pathExists (./private + "/${name}"); path else path;
  privateValue = default: name:
    if withSecrets then import (./private + "/${name}.nix") else default;
  privateFile = name:
    if withSecrets then
      ./private + "/${name}"
    else
      builtins.toFile "missing-secret-file-${name}" "";
}
