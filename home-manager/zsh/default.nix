{pkgs, ... }: {

programs = {
  zsh = {
    enable = true;
    enableAutosuggestions = true;
    enableCompletion = true;
    history = {
      save = 100000;
      size = 100000;
    };
    initExtra = let
      print-pw = pkgs.writeShellScriptBin "print-pw" ''
        pass space/olymp/apollo
        '';
      in
      ''
        export SUDO_ASKPASS="${print-pw}"/bin/print-pw
      '' + builtins.readFile ./zshrc;
    oh-my-zsh = {
      enable = true;
      plugins = [ "colored-man-pages" "git-prompt" ];
    };
  };
};

}
