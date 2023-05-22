{ pkgs, ... }: {
  home.packages = [ pkgs.taskwarrior-git ];
  home.file = {
    "add-git" = {
      target = ".task/hooks/on-add.git";
      text = ''
        export PATH=${pkgs.git}/bin:$PATH
        exec ${pkgs.taskwarrior-git}/bin/taskwarrior-git on-add'';
      executable = true;
    };
    "modify-git" = {
      target = ".task/hooks/on-modify.git";
      text = ''
        export PATH=${pkgs.git}/bin:$PATH
        exec ${pkgs.taskwarrior-git}/bin/taskwarrior-git on-modify'';
      executable = true;
    };
  };
}
