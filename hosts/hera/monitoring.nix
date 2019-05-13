{ config, ... }:
let
  inherit (config.m-0) hosts;
in
{
services.prometheus.exporters.node = {
  firewallFilter = "! -i ens18 -p tcp -m tcp --dport 9100";
};
m-0.monitoring = [
  { name = "hera"; host = "hera-intern:9100";  }
  { name = "monitoring-container"; host = "localhost:9100"; }
];

containers.monitoring = {
  autoStart = true;
  privateNetwork = true;
  hostBridge = "bridge";
  config = { pkgs, lib, ... }: {
    imports = [
      ../../system
    ];
    networking = {
      interfaces.eth0 = {
        ipv6.addresses = [{ address = hosts.monitoring; prefixLength = 112; }];
        ipv4.addresses = [{ address = hosts.monitoring-intern-v4; prefixLength = 24; }];
      };
      inherit (config.networking) nameservers;
      defaultGateway6 = { address = hosts.hera-intern; interface = "eth0"; };
      defaultGateway = { address = hosts.hera-intern-v4; interface = "eth0"; };
      firewall.allowedTCPPorts = [ 9090 9093 ];
    };
    services.prometheus = {
      enable = true;
      rules = [
                ''
          ALERT node_down
          IF (up{name!="apollo"} == 0)
          FOR 5m
          LABELS {
            severity="page"
          }
          ANNOTATIONS {
            summary = "{{$labels.name}}: Node is down.",
            description = "{{$labels.name}} has been down for more than 5 minutes."
          }
          ALERT node_systemd_service_failed
          IF node_systemd_unit_state{state="failed"} == 1
          FOR 4m
          LABELS {
            severity="page"
          }
          ANNOTATIONS {
            summary = "{{$labels.name}}: Service {{$labels.exported_name}} failed.",
            description = "{{$labels.name}} failed to (re)start service {{$labels.exported_name}}."
          }
          ALERT node_filesystem_full_90percent
          IF sort(node_filesystem_free{device!="ramfs"} < node_filesystem_size{device!="ramfs"} * 0.1) / 1024^3
          FOR 5m
          LABELS {
            severity="page"
          }
          ANNOTATIONS {
            summary = "{{$labels.alias}}: Filesystem is running out of space soon.",
            description = "{{$labels.alias}} device {{$labels.device}} on {{$labels.mountpoint}} got less than 10% space left on its filesystem."
          }
          ALERT node_filesystem_full_in_4h
          IF predict_linear(node_filesystem_free{device!="ramfs"}[1h], 4*3600) <= 0
          FOR 5m
          LABELS {
            severity="page"
          }
          ANNOTATIONS {
            summary = "{{$labels.alias}}: Filesystem is running out of space in 4 hours.",
            description = "{{$labels.alias}} device {{$labels.device}} on {{$labels.mountpoint}} is running out of space of in approx. 4 hours"
          }
          ALERT node_filedescriptors_full_in_3h
          IF predict_linear(node_filefd_allocated[1h], 3*3600) >= node_filefd_maximum
          FOR 20m
          LABELS {
            severity="page"
          }
          ANNOTATIONS {
            summary = "{{$labels.alias}} is running out of available file descriptors in 3 hours.",
            description = "{{$labels.alias}} is running out of available file descriptors in approx. 3 hours"
          }
          ALERT node_load1_90percent
          IF node_load1 / on(alias) count(node_cpu{mode="system"}) by (alias) >= 0.9
          FOR 1h
          LABELS {
            severity="page"
          }
          ANNOTATIONS {
            summary = "{{$labels.alias}}: Running on high load.",
            description = "{{$labels.alias}} is running with > 90% total load for at least 1h."
          }
          ALERT node_cpu_util_90percent
          IF 100 - (avg by (alias) (irate(node_cpu{mode="idle"}[5m])) * 100) >= 90
          FOR 1h
          LABELS {
            severity="page"
          }
          ANNOTATIONS {
            summary = "{{$labels.alias}}: High CPU utilization.",
            description = "{{$labels.alias}} has total CPU utilization over 90% for at least 1h."
          }
          ALERT node_ram_using_90percent
          IF node_memory_MemFree + node_memory_Buffers + node_memory_Cached < node_memory_MemTotal * 0.1
          FOR 30m
          LABELS {
            severity="page"
          }
          ANNOTATIONS {
            summary="{{$labels.alias}}: Using lots of RAM.",
            description="{{$labels.alias}} is using at least 90% of its RAM for at least 30 minutes now.",
          }
          ALERT node_swap_using_80percent
          IF node_memory_SwapTotal - (node_memory_SwapFree + node_memory_SwapCached) > node_memory_SwapTotal * 0.8
          FOR 10m
          LABELS {
            severity="page"
          }
          ANNOTATIONS {
            summary="{{$labels.alias}}: Running out of swap soon.",
            description="{{$labels.alias}} is using 80% of its swap space for at least 10 minutes now."
          }
        ''
        ];
      scrapeConfigs = [
        {
          job_name = "nodes";
          static_configs = map (entry: {
            targets = [ entry.host ];
            labels = {"name" = entry.name; };
          }) config.m-0.monitoring;
        }
      ];
      alertmanagerURL = [ "http://localhost:9093" ];
      alertmanager = {
        enable = true;
        listenAddress = "0.0.0.0";
        configuration = {
          "global" = {
            "smtp_smarthost" = "hera.m-0.eu:587";
            "smtp_from" = "alertmanager@m-0.eu";
            "smtp_auth_username" = "alertmanager@m-0.eu";
            "smtp_auth_password" = config.m-0.private.alertmanager-mail-pw;
          };
          "route" = {
            "group_by" = [ "alertname" "alias" ];
            "group_wait" = "30s";
            "group_interval" = "2m";
            "repeat_interval" = "4h";
            "receiver" = "team-admins";
          };
          "receivers" = [
            {
              "name" = "team-admins";
              "email_configs" = [
                {
                  "to" = "malte.brandy@maralorn.de";
                  "send_resolved" = true;
                }
              ];
            }
        ];
      };
    };
      exporters.node.enable = true;
    };
  };
};

}
