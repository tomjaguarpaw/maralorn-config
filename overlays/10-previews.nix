self: super:
let
  unstable = import self.sources.unstable { };
  nixpkgs-master = import self.sources.nixpkgs-master { };
in {
  systemd-next = unstable.systemd;
  gnome3 = super.gnome3 // {
    inherit (unstable.gnome3) gnome-keyring seahorse gdm;
  };
  inherit (unstable)
    aqbanking neovim vimPlugins syncthing nerdfonts ormolu ghcid go-neb fzf nixpkgs-fmt;
  inherit (nixpkgs-master) element-web element-desktop;
}
