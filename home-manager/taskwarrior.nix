{ pkgs, lib, ... }:
{
  home = {
    packages = [ pkgs.taskwarrior ];
    file = {
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
      "eventd/task-${name}.action".text = lib.generators.toINI {} {
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
      "eventd/task.event".text = lib.generators.toINI {} {
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
}
