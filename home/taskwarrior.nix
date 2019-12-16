{ lib, pkgs, config, ... }: {
  services.taskwarrior-sync = {
    enable = true;
    frequency = "*:0/1";
  };
  programs.taskwarrior = let cfg = config.m-0.private.taskwarrior;
  in {
    enable = true;
    dataLocation = "${config.home.homeDirectory}/.task";
    config = {
      taskd = {
        certificate = builtins.toFile "public.cert" cfg.publicCert;
        credentials = cfg.credentials;
        ca = builtins.toFile "ca.cert" cfg.caCert;
        key = builtins.toFile "private.key" cfg.privateKey;
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
    #taskwarrior-on-add-hook = {
    #target = ".task/hooks/on-add.eventd-notification";
    #text = ''
    ##!${pkgs.python3}/bin/python
    #import sys
    #import json
    #import subprocess

    #input_string = sys.stdin.readline()
    #original = json.loads(input_string)
    #command = ['eventc', 'task', 'add']
    #for name, value in original.items():
    #command.append("-d")
    #if type(value) == list:
    #value = ', '.join(value)
    #command.append(name+"='"+str(value)+"'")
    #subprocess.Popen(command)
    #print(input_string)
    #'';
    #executable = true;
    #};
    #taskwarrior-on-modify-hook = {
    #target = ".task/hooks/on-modify.eventd-notification";
    #text = ''
    ##!${pkgs.python3}/bin/python
    #import sys
    #import json
    #import subprocess

    #input_string = sys.stdin.readline()
    #original = json.loads(input_string)
    #input_string = sys.stdin.readline()
    #new = json.loads(input_string)
    #command = ['eventc', 'task', 'modify']
    #for name in set(new.keys()).union(set(original.keys())):
    #if new.get(name) != original.get(name) or name == "description":
    #value = new.get(name, "")
    #command.append("-d")
    #if type(value) == list:
    #value = ', '.join([str(item) for item in value])
    #command.append(name+"='"+str(value)+"'")
    #subprocess.Popen(command)
    #print(input_string)
    #'';
    #executable = true;
    #};
  };
}
