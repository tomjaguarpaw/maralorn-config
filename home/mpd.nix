{ pkgs, config, ... }: {
  services = {
    mpd = {
      enable = true;
      network.listenAddress = "::1";
      musicDirectory = "${config.home.homeDirectory}/media/audio";
      extraConfig = ''
        audio_output {
              type "pulse"
              name "Pulseaudio"
              server "localhost"
        }
      '';
    };
    mpdris2.enable = true;
  };
  systemd.user.services.mpdris2 = {
    Unit.Requires = [ "dbus.service" ];
    Install.WantedBy = [ "default.target" ];
  };
}
