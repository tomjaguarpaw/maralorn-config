{ pkgs, config, ... }: {

  imports = [
    ../../home
    ../../home/on-my-machine.nix
    ../../home/battery.nix
    ../../home/update_tasks.nix
    ../../home/desktop
  ];

  home.packages = builtins.attrValues (import ../../pkgs).laptop-home-pkgs;

  m-0 = {
    hostName = "apollo";
    latex.enable = true;
    accounting = {
      enable = true;
      config = builtins.readFile secret/jaliconfig.py;
    };
    pythia.enable = true;
    unlocker = [{
      name = "hera";
      hostName = "hera-v4";
      pubKey =
        "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQCHkqWlFLtmIlTSKahr2PcL++K75YgfsSU6jwVYW5df3JCkowu/M16SIBxABxYSQrKej5uIz/OFCjqSxHJQ8D5wSYBvn2gYr/BbBcz4rfIJmZ55Od2jckaqlj/M8TtkuPPhsQG7S730vXxK5hbMT8iW5WWv8sIKY/WtaRbZOFMX/53WCLEHtnMu5zFJFWf92+mjIHSLyW8ggl1m525RUiaAfCge2vnuzIFq4kUqJxaWzxIvEWIncKWN10K/HMvdI+yOtbSen41uKedwSFhUFs3xHy1mJddYOrlcJQPt5zuuffZ/nTDVXMZoh5QNwg8ZlkkueVChaS1Y5STjb7cem1Mt";
      passPath = "eu/m-0/hera.m-0.eu/disk";
    }];
    mail = {
      enable = true;
      accounts = config.m-0.private.mail_accounts;
    };
  };

  home.file.".ncmpcpp/config".text = ''
    ask_before_clearing_playlists=no
    mouse_support = yes
    song_columns_list_format = "(24)[red]{a} $R(48)[blue]{t} (24)[green]{b} (4)[magenta]{l}"
    playlist_display_mode = columns
    search_engine_display_mode = columns
    browser_display_mode = columns
    user_interface = alternative
  '';

  programs = {
    firefox = { enable = true; };
    git = {
      signing = {
        signByDefault = true;
        key = "6C3D12CD88CDF46C5EAF4D12226A2D41EF5378C9";
      };
    };
  };

  services = {
    udiskie = {
      enable = true;
      notify = true;
    };
  };
}
