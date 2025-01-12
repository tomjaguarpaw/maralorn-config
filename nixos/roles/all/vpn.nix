{ config, lib, ... }:
let
  interface = "wg-mesh";
  port = 53077;
  inherit (config.networking) hostName;
  prefix = "172.30.77";
  peers = {
    athene = {
      pubkey = "0BCfRqYER62NtSx1tYYTXvR7n20cKKIB3NKAA9sM72U=";
      address = "${prefix}.1";
    };
    hephaistos = {
      pubkey = "Vu3tcouiruqLz0D6M60h7drP2qSiSWRA7roeaUjKQic=";
      address = "${prefix}.2";
    };
    hera = {
      pubkey = "+1s5MDauFG6MdX/k518x/+KAaglJbqC19NB2pzmz8U8=";
      endpoint = "hera.maralorn.de:${toString port}";
      address = "${prefix}.3";
    };
    zeus = {
      pubkey = "iNxbk3ZtsGuZKyWTwZLN2olid/9K3/EkaD7kCNIJr2c=";
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
