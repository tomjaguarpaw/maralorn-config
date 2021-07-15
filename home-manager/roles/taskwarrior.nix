{ lib, pkgs, config, ... }: {
  home.packages = [ pkgs.taskwarrior-git ];
  services.taskwarrior-sync = {
    enable = true;
    frequency = "*:0/1";
  };
  systemd.user.services.watch-tasks = {
    Unit.Description = "Watch tasks for changes and trigger sync";
    Service = {
      ExecStart = toString (
        pkgs.writeShellScript "watch-vdir" ''
          while sleep 1s; do
            ${pkgs.taskwarrior}/bin/task sync
            ${pkgs.inotify-tools}/bin/inotifywait -e move,create,delete,modify -r ${config.home.homeDirectory}/.task
          done
        ''
      );
    };
    Install.WantedBy = [ "default.target" ];
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
  };
  programs.taskwarrior = {
    enable = true;
    dataLocation = "${config.home.homeDirectory}/.task";
    config = {
      taskd = {
        certificate = pkgs.privatePath "taskwarrior/public.cert";
        credentials = pkgs.privateValue "" "taskwarrior/credentials";
        ca = pkgs.privatePath "taskwarrior/ca.cert";
        key = pkgs.privatePath "taskwarrior/private.key";
        server = "hera.m-0.eu:53589";
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
