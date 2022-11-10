{pkgs, ...}: {
  systemd.services.nixpkgs-bot = {
    wantedBy = ["multi-user.target"];
    description = "nixpkgs-bot";
    path = [pkgs.git];
    serviceConfig = {
      WorkingDirectory = "/var/lib/nixpkgs-bot";
      ExecStart = "${pkgs.nixpkgs-bot}/bin/nixpkgs-bot /var/lib/nixpkgs-bot/config.yaml";
      DynamicUser = true;
      StateDirectory = "nixpkgs-bot";
    };
  };
}
