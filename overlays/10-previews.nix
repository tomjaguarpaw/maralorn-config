self: super:
let
  unstable = import self.sources.unstable { };
in {
  gnome3 = super.gnome3 // {
    inherit (unstable.gnome3) gnome-keyring seahorse gdm;
  };
  inherit (unstable)
    neovim vimPlugins syncthing nerdfonts ormolu go-neb; # riot-desktop;
}
