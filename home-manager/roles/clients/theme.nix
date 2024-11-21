{
  imports =
    let
      cpFlake = builtins.getFlake "github:catppuccin/nix/32359bf226fe874d3b7a0a5753d291a4da9616fe";
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
    kitty.catppuccin.enable = true;
    lazygit.catppuccin.enable = true;
    helix.catppuccin.enable = true;
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
