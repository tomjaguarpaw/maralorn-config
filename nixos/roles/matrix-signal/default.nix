{ pkgs, config, lib, ... }:
let
  synapse-port = 8008;

in
{
  imports = [
    ./mautrix-signal-module.nix
  ];

  services.signald.enable = true;

  services.mautrix-signal = {
    enable = true;
    settings = {
      homeserver = {
        address = "https://matrix.maralorn.de";
        domain = "maralorn.de";
      };
      bridge = {
        public_portals = false;
        federate_rooms = false;
        permissions."@maralorn:maralorn.de" = "admin";
        contact_list_names = "allow";
        autocreate_contact_portal = false;
        autocreate_group_portal = true;
      };
    };
  };
}
