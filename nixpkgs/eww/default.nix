_: prev: {
  eww-wayland = prev.eww-wayland.overrideAttrs (drv: rec {
    version = "unstable-2023-12-20";
    src = prev.fetchFromGitHub {
      owner = "elkowar";
      repo = "eww";
      rev = "65d622c81f2e753f462d23121fa1939b0a84a3e0";
      hash = "sha256-MR91Ytt9Jf63dshn7LX64LWAVygbZgQYkcTIKhfVNXI=";
    };
    cargoDeps = drv.cargoDeps.overrideAttrs (_: {
      inherit src;
      outputHash = "sha256-4yeu5AgleZMOLKNynGMd0XuyZxyyZ+RmzNtuJiNPN8g=";
    });
  });
}
