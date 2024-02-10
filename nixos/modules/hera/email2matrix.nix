{ pkgs, ... }:
let
  default_mailbox = {
    MailboxName = "<missing>";
    MatrixRoomId = "<missing>";
    MatrixHomeserverUrl = "https://matrix.maralorn.de";
    MatrixUserId = "@marabot:matrix.maralorn.de";
    MatrixAccessToken = pkgs.privateValue "" "matrix/marabot-token";
    IgnoreSubject = false;
    IgnoreBody = false;
    SkipMarkdown = false;
  };
  email2matrix-config = pkgs.writeText "email2matrix-config.json" (
    builtins.toJSON {
      Smtp = {
        ListenInterface = "[::1]:2525";
        Hostname = "email2matrix.maralorn.de";
        Workers = 10;
      };
      Matrix = {
        Mappings = [
          (
            default_mailbox
            // {
              MailboxName = "notify";
              MatrixRoomId = "!kTKVQjRwxjaoMQmcve:maralorn.de";
            }
          )
          (
            default_mailbox
            // {
              MailboxName = "subjects";
              MatrixRoomId = "!kTKVQjRwxjaoMQmcve:maralorn.de";
              IgnoreBody = true;
            }
          )
          (
            default_mailbox
            // {
              MailboxName = "weather";
              MatrixRoomId = "!ELeFcSrHXgMqOmwnxg:maralorn.de";
            }
          )
        ];
      };
      Misc = {
        Debug = true;
      };
    }
  );
in
{
  systemd.services.email2matrix = {
    script = "${pkgs.email2matrix}/bin/devture-email2matrix --config ${email2matrix-config}";
    wantedBy = [ "multi-user.target" ];
  };
}
