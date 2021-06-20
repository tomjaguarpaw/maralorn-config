final: prev: {
  mumble = (import (prev.applyPatches { src = prev.sources.nixos-unstable; name = "mumble-patched-nixos-unstable"; patches = [ ./0001-mumble-1.3.4-1.4.0pre.patch ]; }) { }).mumble;
}
