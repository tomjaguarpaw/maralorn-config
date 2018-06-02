{ pkgs, ... }:
{

imports = [
  ../../home-manager
];

m-0.laptop.enable = true;
m-0.sleep-nag.enable = true;
m-0.battery.enable = true;
m-0.latex.enable = true;
m-0.accounting.enable = true;
m-0.graphical.enable = true;
m-0.rustdev.enable = true;
m-0.taskwarrior.enable = true;
m-0.eventd.enable = true;
m-0.unlocker = [ {
  name = "hera";
  hostName = "hera.m-0.eu";
  pubKey = "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQCHkqWlFLtmIlTSKahr2PcL++K75YgfsSU6jwVYW5df3JCkowu/M16SIBxABxYSQrKej5uIz/OFCjqSxHJQ8D5wSYBvn2gYr/BbBcz4rfIJmZ55Od2jckaqlj/M8TtkuPPhsQG7S730vXxK5hbMT8iW5WWv8sIKY/WtaRbZOFMX/53WCLEHtnMu5zFJFWf92+mjIHSLyW8ggl1m525RUiaAfCge2vnuzIFq4kUqJxaWzxIvEWIncKWN10K/HMvdI+yOtbSen41uKedwSFhUFs3xHy1mJddYOrlcJQPt5zuuffZ/nTDVXMZoh5QNwg8ZlkkueVChaS1Y5STjb7cem1Mt";
  passPath = "eu/m-0/hera/disk";
} ];

}
