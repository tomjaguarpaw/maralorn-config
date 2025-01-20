{ config, lib, ... }:
let
  interface = "wg-mesh";
  port = 53077;
  inherit (config.networking) hostName;
  prefix = "172.30.77";
  peers = {
    athene = {
      pubkey = "atheMO5T0C1EHJGgXaR9EUIf93idHUEIIF2HC1/pBRk=";
      address = "${prefix}.1";
    };
    hephaistos = {
      pubkey = "hephOehqoB1bm3Go2mTo92fzlI133N4lxROG8X7x3Wo=";
      address = "${prefix}.2";
    };
    hera = {
      pubkey = "hera2vGWsXZTNhHB3awPvv0mrrKlg54uvLDWlyMQkEc=";
      endpoint = "hera.maralorn.de:${toString port}";
      address = "${prefix}.3";
    };
    zeus = {
      pubkey = "zeusu/Qu/8pTCi81IeCmCs7Og9CLfivqbQs+I+RELmc=";
      address = "${prefix}.4";
    };
  };
in
{
  services.wgautomesh = {
    enable = true;
    gossipSecretFile = config.age.secrets."wg/gossip-secret".path;
    settings = {
      inherit interface;
      peers = builtins.attrValues (builtins.removeAttrs peers [ hostName ]);
    };
  };
  networking.firewall.allowedUDPPorts = [
    port
  ];
  systemd.network = {
    netdevs."50-${interface}" = {
      netdevConfig = {
        Kind = "wireguard";
        Name = interface;
      };
      wireguardConfig = {
        PrivateKeyFile = config.age.secrets."wg/${hostName}-private-key".path;
        ListenPort = port;
      };
    };
    networks.${interface} = {
      matchConfig.Name = interface;
      address = [ "${peers.${hostName}.address}/24" ];
    };
  };
  environment = lib.mkIf config.has-persistence {
    persistence.unsnapshoted.directories = [ "/var/lib/private/wgautomesh" ];
  };
}
