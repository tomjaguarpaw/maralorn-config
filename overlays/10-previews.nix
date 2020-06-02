self: super:
let
  unstable = import self.sources.unstable { };
  master = import super.sources.nixpkgs-master { };
in {
  gnome3 = super.gnome3 // {
    inherit (unstable.gnome3) gnome-keyring seahorse gdm;
  };
  inherit (unstable)
    neovim vimPlugins syncthing nerdfonts; # riot-desktop;
  inherit (master) go-neb;
}
