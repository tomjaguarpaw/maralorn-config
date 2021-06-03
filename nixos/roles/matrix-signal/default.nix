{ pkgs, config, lib, ... }:
let
  synapse-port = 8008;

in
{
  imports = [
    ./signald-module.nix
    ./mautrix-signal-module.nix
  ];

  services.signald.enable = true;

  services.mautrix-signal = {
    enable = true;
    settings = {
      homeserver = {
        address = "http://localhost:${builtins.toString synapse-port}";
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
