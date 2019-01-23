{ lib, pkgs, config, ...}:
with lib;
let
  tasksync = pkgs.writeShellScriptBin "tasksync" ''
    cd ${config.home.homeDirectory}/.task
    ${pkgs.git}/bin/git add completed.data pending.data > /dev/null
    ${pkgs.git}/bin/git commit -m 'Updating task data' > /dev/null
    ${pkgs.git}/bin/git pull -X ${if config.m-0.taskwarrior.git_active then "ours" else "theirs"} | ${pkgs.gnugrep}/bin/grep -v "Already up to date."
    ${pkgs.taskwarrior}/bin/task diagnostics | ${pkgs.gnugrep}/bin/grep "Found duplicate" | ${pkgs.gnused}/bin/sed 's/.*Found duplicate //' | ${pkgs.findutils}/bin/xargs -i ${pkgs.gnused}/bin/sed -i '0,/uuid:"{}"/{/uuid:"{}"/d}' completed.data > /dev/null
    ${pkgs.git}/bin/git add completed.data > /dev/null
    ${pkgs.git}/bin/git commit -m 'Fixing duplicates' > /dev/null
    ${pkgs.git}/bin/git push 2>&1 | ${pkgs.gnugrep}/bin/grep -v "Everything up-to-date"
    true
  '';
in {
options.m-0.taskwarrior.enable = mkEnableOption "Taskwarrior";
options.m-0.taskwarrior.git_active = mkEnableOption "This machine will prefer its own state in case of a merge conflict, if enabled.";
config = mkIf config.m-0.taskwarrior.enable {
  systemd.user = {
    services.tasksync = {
      Unit = {
        Description = "Update tasks";
      };
      Service = {
        ExecStart="/bin/sh ${tasksync}/bin/tasksync";
        Type="oneshot";
      };
    };
    timers.tasksync = {
      Timer = {
        OnCalendar = "*:0/1";
      };
      Install = {
        WantedBy = [ "timers.target" ];
      };
    };
  };
  home = {
    packages = [ pkgs.taskwarrior tasksync pkgs.tasksh ];
    file = {
      ".taskrc".text = ''
        data.location=~/.task
        default.command=default
        alias.inbox=+PENDING -TAGGED limit:1
        alias.inboxall=+PENDING -TAGGED

        verbose=blank,header,footnote,label,new-id,affected,edit,special,sync
        nag=

        report.default.columns=id,tags,priority,description,due,start.active,project
        report.default.description=List tasks
        report.default.filter=status:pending -BLOCKED
        report.default.labels=ID,Tags,,Beschreibung,Bis,Start,Projekt
        report.default.sort=modified-

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

        # Bugwarrior UDAs
        uda.githubtitle.type=string
        uda.githubtitle.label=Github Title
        uda.githubbody.type=string
        uda.githubbody.label=Github Body
        uda.githubcreatedon.type=date
        uda.githubcreatedon.label=Github Created
        uda.githubupdatedat.type=date
        uda.githubupdatedat.label=Github Updated
        uda.githubmilestone.type=string
        uda.githubmilestone.label=Github Milestone
        uda.githubrepo.type=string
        uda.githubrepo.label=Github Repo Slug
        uda.githuburl.type=string
        uda.githuburl.label=Github URL
        uda.githubtype.type=string
        uda.githubtype.label=Github Type
        uda.githubnumber.type=numeric
        uda.githubnumber.label=Github Issue/PR #
        uda.githubuser.type=string
        uda.githubuser.label=Github User
        uda.gitlabtitle.type=string
        uda.gitlabtitle.label=Gitlab Title
        uda.gitlabdescription.type=string
        uda.gitlabdescription.label=Gitlab Description
        uda.gitlabcreatedon.type=date
        uda.gitlabcreatedon.label=Gitlab Created
        uda.gitlabupdatedat.type=date
        uda.gitlabupdatedat.label=Gitlab Updated
        uda.gitlabduedate.type=date
        uda.gitlabduedate.label=Gitlab Due Date
        uda.gitlabmilestone.type=string
        uda.gitlabmilestone.label=Gitlab Milestone
        uda.gitlaburl.type=string
        uda.gitlaburl.label=Gitlab URL
        uda.gitlabrepo.type=string
        uda.gitlabrepo.label=Gitlab Repo Slug
        uda.gitlabtype.type=string
        uda.gitlabtype.label=Gitlab Type
        uda.gitlabnumber.type=numeric
        uda.gitlabnumber.label=Gitlab Issue/MR #
        uda.gitlabstate.type=string
        uda.gitlabstate.label=Gitlab Issue/MR State
        uda.gitlabupvotes.type=numeric
        uda.gitlabupvotes.label=Gitlab Upvotes
        uda.gitlabdownvotes.type=numeric
        uda.gitlabdownvotes.label=Gitlab Downvotes
        uda.gitlabwip.type=numeric
        uda.gitlabwip.label=Gitlab MR Work-In-Progress Flag
        uda.gitlabauthor.type=string
        uda.gitlabauthor.label=Gitlab Author
        uda.gitlabassignee.type=string
        uda.gitlabassignee.label=Gitlab Assignee
        uda.gitlabnamespace.type=string
        uda.gitlabnamespace.label=Gitlab Namespace
        uda.gitlabweight.type=numeric
        uda.gitlabweight.label=Gitlab Weight
        # END Bugwarrior UDAs
      '';
      taskwarrior-on-add-hook = {
        target = ".task/hooks/on-add.eventd-notification";
        text = ''
          #!${pkgs.python3}/bin/python
          import sys
          import json
          import subprocess

          input_string = sys.stdin.readline()
          original = json.loads(input_string)
          command = ['eventc', 'task', 'add']
          for name, value in original.items():
            command.append("-d")
            if type(value) == list:
              value = ', '.join(value)
            command.append(name+"='"+str(value)+"'")
          subprocess.Popen(command)
          print(input_string)
          '';
        executable = true;
      };
      taskwarrior-on-modify-hook = {
        target = ".task/hooks/on-modify.eventd-notification";
        text = ''
          #!${pkgs.python3}/bin/python
          import sys
          import json
          import subprocess

          input_string = sys.stdin.readline()
          original = json.loads(input_string)
          input_string = sys.stdin.readline()
          new = json.loads(input_string)
          command = ['eventc', 'task', 'modify']
          for name in set(new.keys()).union(set(original.keys())):
            if new.get(name) != original.get(name) or name == "description":
              value = new.get(name, "")
              command.append("-d")
              if type(value) == list:
                value = ', '.join([str(item) for item in value])
              command.append(name+"='"+str(value)+"'")
          subprocess.Popen(command)
          print(input_string)
          '';
        executable = true;
      };
    };
  };
  xdg = let
    taskAction = name: template: {
      "eventd/task-${name}.action".text = generators.toINI {} {
        Action = {
          Name = "task-${name}";
        };
        Notification = {
          Text = template;
        };
        NotificationBubble = {
          Queue = "tasks";
        };
      };
    };
  in {
    configFile = {
      "eventd/task.event".text = generators.toINI {} {
        "Event task add" = {
          Actions = "task-new";
        };
        "Event task modify" = {
          Actions = "task-changed";
        };
      };
    } //
    taskAction "changed" "Changes in task:\\n<b>\${description}</b>\${status:+\\nStatus: }\${status}\${tags:+\\nTags: }\${tags}\${project:+\\nProject: }\${project}" //
    taskAction "new" "New \${status} task\${tags:! in inbox}:\\n<b>\${description}</b>\${tags:+\\nTags: }\${tags}\${project:+\\nProject: }\${project}";
  };
};

}
