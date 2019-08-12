{ pkgs, config, ... }:
let
  inherit (import ../pkgs) email2matrix;
  default_mailbox = {
    MailboxName = "<missing>";
    MatrixRoomId = "<missing>";
    MatrixHomeserverUrl = "https://matrix.maralorn.de";
    MatrixUserId = "@marabot:matrix.maralorn.de";
    MatrixAccessToken = config.m-0.private.matrix_marabot_token;
    IgnoreSubject = false;
    IgnoreBody = false;
    SkipMarkdown = true;
  };
  email2matrix-config = pkgs.writeText "email2matrix-config.json"
    (builtins.toJSON {
      Smtp = {
        ListenInterface = "[::1]:2525";
        Hostname = "email2matrix.maralorn.de";
        Workers = 10;
      };
      Matrix = {
        Mappings = [
          (default_mailbox // {
            MailboxName = "subjects";
            MatrixRoomId = "!kTKVQjRwxjaoMQmcve:maralorn.de";
            IgnoreBody = true;
          })
          (default_mailbox // {
            MailboxName = "monitoring";
            MatrixRoomId = "!negVsngnYOmXYCLKiO:maralorn.de";
          })
          (default_mailbox // {
            MailboxName = "weather";
            MatrixRoomId = "!ELeFcSrHXgMqOmwnxg:maralorn.de";
          })
        ];
      };
      Misc = { Debug = true; };
    });
in {
  systemd.services.email2matrix = {
    script =
      "${email2matrix}/bin/devture-email2matrix --config ${email2matrix-config}";
    wantedBy = [ "multi-user.target" ];
  };
}
