final: prec: {
  withSecrets = let val = builtins.pathExists ../private/submodule-is-checked-out;
  in builtins.trace
  (if val then "Building _with_ secrets!" else "Building _without_ secrets!")
  val;
  privatePath = name:
    let path = "/etc/nixos/private/${name}";
    in if final.withSecrets then
      assert builtins.pathExists path; path
    else
      path;
  privateValue = default: name:
    if final.withSecrets then import (../private + "/${name}.nix") else default;
  privateFile = name:
    if final.withSecrets then ../private + "/${name}" else builtins.toFile "missing-secret-file-${name}" "";
}
