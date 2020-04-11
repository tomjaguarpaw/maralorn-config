{ pkgs, config, ... }: {

  imports = [ ../../home ../../home/on-my-machine.nix ./weechat ./secret ];

  m-0 = {
    hostName = "hera";
    mail = {
      enable = true;
      accounts = config.m-0.private.mail_accounts;
    };
  };
  services = {
    mpd = {
      enable = true;
      network.listenAddress = "::1";
      musicDirectory = "/media/audio";
      extraConfig = ''
        audio_output {
         type "httpd"
         name "HTTP Stream"
         encoder "vorbis"
         port "8666"
         bitrate "192"
         format "44100:16:1"
         always_on "yes" # prevent MPD from disconnecting all listeners when playback is stopped.
         tags "yes" # httpd supports sending tags to listening streams.
        }
      '';
    };
  };
  systemd.user = {
    services = {
      kassandra = {
        Unit = { Description = "Kassandra Server"; };
        Service = {
          WorkingDirectory = "/var/www/kassandra";
          Type = "forking";
          ExecStart = "/var/www/kassandra/backend";
          Restart = "always";
          Environment = "PATH=${pkgs.coreutils}/bin/:${pkgs.taskwarrior}/bin";
        };
        Install = { WantedBy = [ "default.target" ]; };
      };
    };
  };

}
