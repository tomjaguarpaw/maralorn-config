{ pkgs, config, ... }:
{

imports = [
  ../../home-manager
];

m-0 = {
  hostName = "apollo";
  laptop.enable = true;
  sleep-nag.enable = true;
  battery.enable = true;
  latex.enable = true;
  accounting = {
    enable = true;
    config = builtins.readFile secret/jaliconfig.py;
  };
  graphical.enable = true;
  rustdev.enable = true;
  taskwarrior = {
    enable = true;
    git_active = true;
  };
  update_tasks.enable = true;
  eventd.enable = true;
  pythia.enable = true;
  unlocker = [ {
    name = "hera";
    hostName = "hera-v4.m-0.eu";
    pubKey = "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQCHkqWlFLtmIlTSKahr2PcL++K75YgfsSU6jwVYW5df3JCkowu/M16SIBxABxYSQrKej5uIz/OFCjqSxHJQ8D5wSYBvn2gYr/BbBcz4rfIJmZ55Od2jckaqlj/M8TtkuPPhsQG7S730vXxK5hbMT8iW5WWv8sIKY/WtaRbZOFMX/53WCLEHtnMu5zFJFWf92+mjIHSLyW8ggl1m525RUiaAfCge2vnuzIFq4kUqJxaWzxIvEWIncKWN10K/HMvdI+yOtbSen41uKedwSFhUFs3xHy1mJddYOrlcJQPt5zuuffZ/nTDVXMZoh5QNwg8ZlkkueVChaS1Y5STjb7cem1Mt";
    passPath = "eu/m-0/hera/disk";
  } ];
  mail = {
    enable = true;
    accounts = config.m-0.private.mail_accounts;
  };
};


programs.autorandr = {
  enable = true;
  hooks = {
    postswitch = {
     "restart-i3" = "${pkgs.i3}/bin/i3-msg restart";
     "update-background" = "${pkgs.systemd}/bin/systemctl --user restart random-background.service";
    };
  };
  profiles = {
    "home" = {
      fingerprint = {
        "DP2-2" = "00ffffffffffff00046997244a2e00001615010380351e782a6045a6564a9c25125054bf6f00714f814081809500b300d1c081c08100023a801871382d40582c4500132b2100001e000000ff0042364c4d54463031313835300a000000fd00324b1e5011000a202020202020000000fc00415355532056573234380a20200052";
        "eDP1" = "00ffffffffffff0006af362300000000001b0104a51f117802f4f5a4544d9c270f505400000001010101010101010101010101010101e65f00a0a0a040503020350035ae100000180000000f0000000000000000000000000020000000fe0041554f0a202020202020202020000000fe004231343051414e30322e33200a00b2";
        "DP2-1" = "00ffffffffffff0005b70000570500000a120103082a1a782ae5b5a355499927135054bfef809500950f8140718f01010101010101019a29a0d05184223050983600a4001100001c000000ff003030313336370a202020202020000000fd00374b1e500e000a202020202020000000fc0058313931305744530a2020202000bf";
      };
      config = {
        eDP1 = {
          enable = true;
          primary = true;
          position = "0x0";
          mode = "2560x1440";
        };
        DP2-2 = {
          enable = true;
          position = "4000x0";
          mode = "1920x1080";
        };
        DP2-1 = {
          enable = true;
          position = "2560x0";
          mode = "1440x900";
        };
      };
    };
    "work" = {
      fingerprint = {
        "DP2" = "00ffffffffffff0009d1ce7845540000101a01030e351e782e6b35a455559f270c5054a56b80d1c081c081008180a9c0b30001010101023a801871382d40582c4500132b2100001e000000ff005334473034343238534c300a20000000fd00324c1e5315000a202020202020000000fc0042656e5120474c323436300a2000e2";
        "eDP1" = "00ffffffffffff0006af362300000000001b0104a51f117802f4f5a4544d9c270f505400000001010101010101010101010101010101e65f00a0a0a040503020350035ae100000180000000f0000000000000000000000000020000000fe0041554f0a202020202020202020000000fe004231343051414e30322e33200a00b2";
      };
      config = {
        eDP1 = {
          enable = true;
          primary = true;
          position = "0x0";
          mode = "2560x1440";
        };
        DP2 = {
          enable = true;
          position = "2560x0";
          mode = "1920x1080";
        };
      };
    };
    "default" = {
      fingerprint = {
        "eDP1" = "00ffffffffffff0006af362300000000001b0104a51f117802f4f5a4544d9c270f505400000001010101010101010101010101010101e65f00a0a0a040503020350035ae100000180000000f0000000000000000000000000020000000fe0041554f0a202020202020202020000000fe004231343051414e30322e33200a00b2";
      };
      config = {
        eDP1 = {
          enable = true;
          primary = true;
          position = "0x0";
          mode = "2560x1440";
        };
      };
    };
  };
};

home.packages = [
  (pkgs.writeShellScriptBin "maintenance" ''
    sudo -A systemctl start nixos-upgrade
    sudo -A systemctl start nix-gc
    sudo -A systemctl start nix-optimise
    sudo -A systemctl start borgbackup-job-data.service
  '')
];

}
