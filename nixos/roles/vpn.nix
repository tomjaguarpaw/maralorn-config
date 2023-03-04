{
  config,
  lib,
  pkgs,
  ...
}: {
  services.tailscale.enable = true;
  networking = {
    firewall = {
      extraInputRules = ''
        meta iifname m0wire accept comment "wireguard vpn"
        meta iifname tailscale0 accept comment "headscale vpn"
      '';
      checkReversePath = "loose";
      allowedUDPPorts = [config.services.tailscale.port];
    };
  };

  # 2023-03-03: https://tailscale.com/blog/nixos-minecraft/
  # create a oneshot job to authenticate to Tailscale
  systemd.services.tailscale-autoconnect = {
    description = "Automatic connection to Tailscale";

    # make sure tailscale is running before trying to connect to tailscale
    after = ["network-pre.target" "tailscale.service"];
    wants = ["network-pre.target" "tailscale.service"];
    wantedBy = ["multi-user.target"];

    # set this service as a oneshot job
    serviceConfig.Type = "oneshot";

    # have the job run this shell script
    script = ''
      # wait for tailscaled to settle
      sleep 2

      # check if we are already authenticated to tailscale
      status="$(${lib.getExe pkgs.tailscale} status -json | ${lib.getExe pkgs.jq} -r .BackendState)"
      if [ $status = "Running" ]; then # if so, then do nothing
        exit 0
      fi

      # otherwise authenticate with tailscale
      ${lib.getExe pkgs.tailscale} up -authkey file:${config.age.secrets."tailscale-preauthkey".path} -login-server https://headscale.maralorn.de
    '';
  };
}
