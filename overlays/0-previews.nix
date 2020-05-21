self: super:
let
  preview = version: func:
    if super.lib.versionOlder super.lib.version version then
      func (import <unstable> { })
    else
      { };
in preview "20.09pre-git" (unstable: {
  gnome3 = super.gnome3 // {
    inherit (unstable.gnome3) gnome-keyring seahorse gdm;
  };
  inherit (unstable) neovim vimPlugins syncthing nerdfonts;
})
