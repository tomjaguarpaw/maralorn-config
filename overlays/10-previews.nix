self: super:
let
  unstable = import self.sources.unstable { };
in {
  systemd-next = unstable.systemd;
  gnome3 = super.gnome3 // {
    inherit (unstable.gnome3) gnome-keyring seahorse gdm;
  };
  inherit (unstable)
    aqbanking neovim vimPlugins syncthing nerdfonts ormolu ghcid go-neb element-web element-desktop;
}
