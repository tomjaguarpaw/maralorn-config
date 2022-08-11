(import (import ./nix/sources.nix).nixos-stable {}).mkShell {
  shellHook = ''
    ${(import ./test.nix).pre-commit-check.shellHook}
  '';
}
