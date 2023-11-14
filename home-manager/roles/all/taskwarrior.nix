{
  pkgs,
  config,
  lib,
  ...
}:
let
  fix-tasks = pkgs.writeShellScriptBin "fix-tasks" ''
    sed 's/depends.*open.*\([0-9a-f]\{8\}-[0-9a-f]\{4\}-[0-9a-f]\{4\}-[0-9a-f]\{4\}-[0-9a-f]\{12\}\).*close[^ ]* /depends:"\1" /' -i ~/.task/*.data
    sed 's/dep_\[[^ ]* //' -i ~/.task/*.data
  '';
in
{
  home.packages = [ fix-tasks ];
  services.taskwarrior-sync = {
    enable = true;
    frequency = "*:0/1";
  };
  systemd.user.services = {
    taskwarrior-sync.Service.ExecStartPre =
      # script has been generated with
      # ssh root@hera nixos-taskserver user export maralorn.de maralorn
      (pkgs.writeShellScript "ensure-taskwarrior-login" ''
        set -eu
        export PATH=${
          lib.makeBinPath [
            pkgs.taskwarrior
            pkgs.coreutils
            pkgs.gnugrep
          ]
        }
        if [[ -z "$(${lib.getExe pkgs.taskwarrior} show taskd.credentials | grep maralorn)" ]]; then
          yes | /bin/sh /run/agenix/taskwarrior-creds-script
        fi
      '').outPath;
    watch-tasks = {
      Unit.Description = "Watch tasks for changes and trigger sync";
      Service = {
        ExecStart =
          (pkgs.writeShellScript "watch-vdir" ''
            while ${pkgs.coreutils}/bin/sleep 1s; do
              ${pkgs.systemd}/bin/systemctl --user start taskwarrior-sync
              ${pkgs.inotify-tools}/bin/inotifywait -e move,create,delete,modify -r ${config.home.homeDirectory}/.task
            done
          '').outPath;
      };
      Install.WantedBy = [ "default.target" ];
    };
  };
  home.file = {
    "add-kassandra-notification" = {
      target = ".task/hooks/on-add.kassandra-notification";
      executable = true;
      text = ''
        #!${pkgs.bash}/bin/bash
        ${pkgs.coreutils}/bin/tee >(${pkgs.netcat}/bin/nc 127.0.0.1 6545)
      '';
    };
    "modify-kassandra-notification" = {
      target = ".task/hooks/on-modify.kassandra-notification";
      executable = true;
      text = ''
        #!${pkgs.bash}/bin/bash
        ${pkgs.coreutils}/bin/tail -n 1 | ${pkgs.coreutils}/bin/tee >(${pkgs.netcat}/bin/nc 127.0.0.1 6545)
      '';
    };
    "modify-done-habitica-points" = {
      target = ".task/hooks/on-modify.habitica-points";
      executable = true;
      source = "${
          pkgs.writeHaskellScript
            {
              name = "habitica-points";
              bins = [
                pkgs.curl
                pkgs.jq
                pkgs.libnotify
              ];
              imports = [ "Data.Aeson" ];
            }
            ''

              data Task = Task { status :: Text } deriving (Generic, FromJSON)

              main = do
                oldTask <- getLine
                newTask <- getLine
                let oldStatus = maybe "unknown" status (decode (encodeUtf8 oldTask))
                    newStatus = maybe "unknown" status (decode (encodeUtf8 newTask))
                when (oldStatus /= "completed" && newStatus == "completed") $ do
                  result :: String <- curl "-XPOST" "-H" "x-api-user: dbd97aba-8b6b-4649-9dd4-dad284333925" "-H" "x-api-key: ${
                    pkgs.privateValue "" "habitica-token"
                  }" "https://habitica.com/api/v3/tasks/6e95cccd-06e1-466c-b871-643dff31423c/score/up" |> jq "-c" ".data._tmp" |> captureTrim <&> decodeUtf8
                  notify_send "Task Completed!" result
                putTextLn newTask
            ''
        }/bin/habitica-points";
    };
  };
  programs.taskwarrior = {
    enable = true;
    dataLocation = "${config.home.homeDirectory}/.task";
    config = {
      taskd = {
        server = "taskserver.maralorn.de:53589";
      };
    };
    extraConfig = ''
      alias.inbox=+PENDING -TAGGED limit:1
      alias.inboxall=+PENDING -TAGGED

      verbose=blank,header,footnote,label,new-id,affected,edit,special,sync
      nag=


      uda.partof.type=string
      uda.partof.label=Parent task
      uda.generated.type=string
      uda.gen_name.type=string
      uda.gen_name.label=Generator name
      uda.gen_id.type=string
      uda.gen_id.label=Generator id
      uda.gen_orphan.type=string
      uda.gen_orphan.label=Generated orphan behavior
      uda.listposition.type=numeric
    '';
  };
}
