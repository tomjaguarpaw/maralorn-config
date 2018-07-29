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
  taskwarrior.enable = true;
  update_tasks.enable = true;
  eventd.enable = true;
  unlocker = [ {
    name = "hera";
    hostName = "hera.m-0.eu";
    pubKey = "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQCHkqWlFLtmIlTSKahr2PcL++K75YgfsSU6jwVYW5df3JCkowu/M16SIBxABxYSQrKej5uIz/OFCjqSxHJQ8D5wSYBvn2gYr/BbBcz4rfIJmZ55Od2jckaqlj/M8TtkuPPhsQG7S730vXxK5hbMT8iW5WWv8sIKY/WtaRbZOFMX/53WCLEHtnMu5zFJFWf92+mjIHSLyW8ggl1m525RUiaAfCge2vnuzIFq4kUqJxaWzxIvEWIncKWN10K/HMvdI+yOtbSen41uKedwSFhUFs3xHy1mJddYOrlcJQPt5zuuffZ/nTDVXMZoh5QNwg8ZlkkueVChaS1Y5STjb7cem1Mt";
    passPath = "eu/m-0/hera/disk";
  } ];
  mail = {
    enable = true;
    boxes = with config.m-0.private.mail; [ private work work2 ];
    sendmail = with config.m-0.private.sendmail; [ private work work2 club club2 ];
    default = config.m-0.private.sendmail.private;
  };
};
home.packages = [
  (pkgs.writeShellScriptBin "maintenance" ''
    sudo -A nix-channel --update
    sleep 5s
    sudo -A systemctl start nixos-upgrade
    sudo -A systemctl start nix-gc
    sudo -A systemctl start nix-optimise
    sudo -A systemctl start borgbackup-job-data.service
  '')
];

}
