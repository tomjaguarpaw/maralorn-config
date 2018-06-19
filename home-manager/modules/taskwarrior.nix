{ lib, pkgs, config, ...}:
with lib;
{
options.m-0.taskwarrior.enable = mkEnableOption "Taskwarrior";
config = mkIf config.m-0.taskwarrior.enable {
  home = {
    packages = [ pkgs.taskwarrior ];
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
        uda.partof.label=parent task
        uda.generated.type=string
        uda.gen_name.type=string
        uda.gen_name.label=generator name
        uda.gen_id.type=string
        uda.gen_id.label=generator id
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
            command.append(name+"='"+value+"'")
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
                value = ', '.join(value)
              command.append(name+"='"+value+"'")
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
