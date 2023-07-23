{
  programs.hyprland.enable = true;
  services.xserver = {
    enable = true;
    displayManager = {
      autoLogin = {
        enable = true;
        user = "maralorn";
      };
      sddm.enable = true;
    };
  };
}
