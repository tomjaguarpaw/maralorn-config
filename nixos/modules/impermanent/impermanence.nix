{

  environment.etc = {
    nixos.source = "/disk/persist/home/maralorn/git/config";
    machine-id.source = "/disk/persist/machine-id";
  };

  environment.persistence."/disk/persist" = {
    directories = [
      "/etc/ssh" # ssh
      "/var/lib/nixos" # Nixos has state to track userids
      "/var/lib/tailscale" # VPN login state
      "/root/.ssh" # ssh
      "/var/lib/bluetooth" # Bluetooth pairing data
      "/var/lib/acme" # Persist CA ceritificates
    ];
  };
  environment.persistence."/disk/volatile" = {
    users.maralorn.directories = [
      ".local/state/wireplumber" # For volume levels
    ];
  };
}
