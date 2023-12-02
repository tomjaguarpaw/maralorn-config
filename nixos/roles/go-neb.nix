{pkgs, ...}:
{
  services.go-neb = {
    enable = true;
    baseUrl = "http://localhost";
    config = {
      clients = [
        {
          UserId = "@marabot:maralorn.de";
          AccessToken = pkgs.privateValue "" "matrix/marabot-token";
          HomeServerUrl = "https://matrix.maralorn.de";
          Sync = true;
          AutoJoinRooms = true;
          DisplayName = "marabot";
        }
      ];
      realms = [];
      sessions = [];
      services = [
        {
          ID = "alertmanager_service";
          Type = "alertmanager";
          UserId = "@marabot:maralorn.de";
          Config = {
            webhook_url = "http://localhost:4050/services/hooks/YWxlcnRtYW5hZ2VyX3NlcnZpY2UK";
            rooms = {
              "!negVsngnYOmXYCLKiO:maralorn.de" = {
                text_template = ''{{range .Alerts -}} [{{ .Status }}] {{index .Annotations "description"}} ({{index .Labels "alertname" }}){{ end -}}'';
                html_template = ''
                  {{range .Alerts -}}{{ $severity := index .Labels "severity" }}{{ if eq .Status "firing" }}{{ if eq $severity "critical"}}<font color='red'><b>[FIRING - CRITICAL]</b></font>{{ else if eq $severity "warning"}}<font color='orange'><b>[FIRING - WARNING]</b></font>{{ else }}<font color='yellow'><b>[FIRING - {{ $severity }}]</b></font>{{ end }}{{ else }}<font color='green'><b>[RESOLVED]</b></font>{{ end }} {{ index .Annotations "description"}} {{ $url := index .Labels "url" }}{{ if eq $url "" }}{{ else }}<a href="{{ $url }}">more infos</a> {{ end }}({{ index .Labels "alertname"}}, <a href="https://stats.maralorn.de/d/health-status">dashboard</a>, <a href="{{ .SilenceURL }}">silence</a>)<br/>{{end -}}
                '';
                msg_type = "m.text"; # Must be either `m.text` or `m.notice`
              };
            };
          };
        }
      ];
    };
  };
}
