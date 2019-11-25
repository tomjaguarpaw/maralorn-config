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
    taskwarrior = {
      enable = true;
      git_active = true;
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
    network-manager-applet.enable = true;
  };

  programs.autorandr = {
    enable = true;
    hooks = {
      postswitch = {
        "restart-i3" = "${pkgs.i3}/bin/i3-msg restart";
        "update-background" =
          "${pkgs.systemd}/bin/systemctl --user restart random-background.service";
      };
    };
    profiles = {
      "home" = {
        fingerprint = {
          "DP-2-2" =
            "00ffffffffffff00046997244a2e00001615010380351e782a6045a6564a9c25125054bf6f00714f814081809500b300d1c081c08100023a801871382d40582c4500132b2100001e000000ff0042364c4d54463031313835300a000000fd00324b1e5011000a202020202020000000fc00415355532056573234380a20200052";
          "eDP-1" =
            "00ffffffffffff0006af362300000000001b0104a51f117802f4f5a4544d9c270f505400000001010101010101010101010101010101e65f00a0a0a040503020350035ae100000180000000f0000000000000000000000000020000000fe0041554f0a202020202020202020000000fe004231343051414e30322e33200a00b2";
          "DP-2-1" =
            "00ffffffffffff0005b70000570500000a120103082a1a782ae5b5a355499927135054bfef809500950f8140718f01010101010101019a29a0d05184223050983600a4001100001c000000ff003030313336370a202020202020000000fd00374b1e500e000a202020202020000000fc0058313931305744530a2020202000bf";
        };
        config = {
          eDP-1 = {
            enable = true;
            primary = true;
            position = "0x0";
            mode = "2560x1440";
          };
          DP-2-2 = {
            enable = true;
            position = "4000x0";
            mode = "1920x1080";
          };
          DP-2-1 = {
            enable = true;
            position = "2560x0";
            mode = "1440x900";
          };
        };
      };
      "work" = {
        fingerprint = {
          "DP-2" =
            "00ffffffffffff0009d1ce7845540000101a01030e351e782e6b35a455559f270c5054a56b80d1c081c081008180a9c0b30001010101023a801871382d40582c4500132b2100001e000000ff005334473034343238534c300a20000000fd00324c1e5315000a202020202020000000fc0042656e5120474c323436300a2000e2";
          "eDP-1" =
            "00ffffffffffff0006af362300000000001b0104a51f117802f4f5a4544d9c270f505400000001010101010101010101010101010101e65f00a0a0a040503020350035ae100000180000000f0000000000000000000000000020000000fe0041554f0a202020202020202020000000fe004231343051414e30322e33200a00b2";
        };
        config = {
          eDP-1 = {
            enable = true;
            primary = true;
            position = "0x0";
            mode = "2560x1440";
          };
          DP-2 = {
            enable = true;
            position = "2560x0";
            mode = "1920x1080";
          };
        };
      };
      "default" = {
        fingerprint = {
          "eDP-1" =
            "00ffffffffffff0006af362300000000001b0104a51f117802f4f5a4544d9c270f505400000001010101010101010101010101010101e65f00a0a0a040503020350035ae100000180000000f0000000000000000000000000020000000fe0041554f0a202020202020202020000000fe004231343051414e30322e33200a00b2";
        };
        config = {
          eDP-1 = {
            enable = true;
            primary = true;
            position = "0x0";
            mode = "2560x1440";
          };
        };
      };
    };
  };

}
