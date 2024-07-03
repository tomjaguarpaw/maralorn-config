{
  imports =
    let
      cpFlake = builtins.getFlake "github:catppuccin/nix/9345073d27d91ab66c1b6ab65df322906992aa59";
    in
    [ cpFlake.homeManagerModules.catppuccin ];
  catppuccin = {
    enable = true;
    flavor = "latte";
    accent = "blue";
    pointerCursor.enable = true;
  };
  programs = {
    foot.catppuccin.enable = true;
    fzf.catppuccin.enable = true;
    bat = {
      enable = true;
      catppuccin.enable = true;
    };
    btop = {
      enable = true;
      catppuccin.enable = true;
    };
    helix.catppuccin.enable = true;
    lazygit.catppuccin.enable = true;
    mpv.catppuccin.enable = true;
    newsboat.catppuccin.enable = true;
    starship.catppuccin.enable = true;
    swaylock.catppuccin.enable = true;
    tmux.catppuccin.enable = true;
    zsh.syntaxHighlighting.catppuccin.enable = true;
  };
  services.mako.catppuccin.enable = true;
  wayland.windowManager.hyprland.catppuccin.enable = true;
}
