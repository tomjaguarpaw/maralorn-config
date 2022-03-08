(import (import ./nix/sources.nix).nixos-unstable {}).mkShell {
  shellHook = ''
    ${(import ./test.nix).pre-commit-check.shellHook}
  '';
}
