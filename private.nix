let
  persistPath = "/disk/persist/maralorn";
  hasPersistDisk = builtins.pathExists persistPath;
  privateExists = builtins.pathExists private/submodule-is-checked-out;
  var = "WITH_SECRETS";
  explicitUsePrivate = builtins.getEnv var == "true";
  explicitNotUsePrivate = builtins.getEnv var == "false";
  usePrivate = !explicitNotUsePrivate && (explicitUsePrivate || privateExists);
  withSecrets = builtins.trace
    (if usePrivate then
      assert privateExists; "Building _with_ secrets!"
    else
      "Building _without_ secrets!")
    usePrivate;
in
{
  inherit withSecrets;
  privatePath = name:
    let path = "${if hasPersistDisk then persistPath else "/home/maralorn"}/git/config/private/${name}";
    in if withSecrets then assert builtins.pathExists (./private + "/${name}"); path else path;
  privateValue = default: name:
    if withSecrets then import (./private + "/${name}.nix") else default;
  privateFile = name:
    if withSecrets then
      ./private + "/${name}"
    else
      builtins.toFile "missing-secret-file-${name}" "";
}
