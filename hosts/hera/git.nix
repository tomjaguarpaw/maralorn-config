{ config, ... }:
let
  me = config.m-0.private.me;
in {
  containers.git = {
    autoStart = true;
    privateNetwork = true;
    bindMounts = {
      "/home/git" = { hostPath = "/home/maralorn/data/git"; isReadOnly = false; };
    };
    hostBridge = "bridge";
    config = { pkgs, ... }: {
      imports = [../../system];

      users.users.git = {
        isNormalUser = true;
        uid = 1000;
        openssh.authorizedKeys.keys = me.keys;
      };
      networking = {
        interfaces.eth0 = {
          ipv6.addresses = [{ address = config.m-0.hosts.git; prefixLength = 112; }];
        };
        inherit (config.networking) nameservers;
        defaultGateway6 = { address = config.m-0.hosts.hera-intern; interface = "eth0"; };
		  firewall.allowedTCPPorts = [ 80 443 ];
      };

      environment.systemPackages = [ pkgs.git ];


      services = {
        sshd.enable = true;
        fcgiwrap.enable = true;
        nginx = {
          enable = true;
          virtualHosts."git.m-0.eu" = {
            forceSSL = true;
            enableACME = true;
            default = true;
            locations = {
              "~ (/.*)" = {
                extraConfig = ''
						# fcgiwrap is set up to listen on this host:port
						fastcgi_pass  unix:/run/fcgiwrap.sock;
						include       ${pkgs.nginx}/conf/fastcgi_params;
						fastcgi_param SCRIPT_FILENAME     ${pkgs.git}/bin/git-http-backend;
						# export all repositories under GIT_PROJECT_ROOT
						fastcgi_param GIT_HTTP_EXPORT_ALL "";
						fastcgi_param GIT_PROJECT_ROOT    /home/git;
						fastcgi_param PATH_INFO           $1;
                '';
              };
            };
          };
        };
      };
    };
  };
}
