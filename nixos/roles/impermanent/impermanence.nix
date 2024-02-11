{
  environment = {
    etc = {
      nixos.source = "/disk/persist/home/maralorn/git/config";
      machine-id.source = "/disk/persist/machine-id";
    };

    persistence.unsnapshoted.persistentStoragePath = "/disk/volatile";

    persistence.snapshoted = {
      persistentStoragePath = "/disk/persist";
      directories = [
        "/etc/ssh" # ssh
        "/etc/NetworkManager/system-connections"
        "/var/lib/nixos" # Nixos has state to track userids
        "/var/lib/tailscale" # VPN login state
        "/var/lib/bluetooth" # Bluetooth pairing data
        "/var/lib/acme" # Persist CA ceritificates
        "/root/.ssh" # ssh
      ];
    };
  };
}
