{ pkgs, ... }:
{
  environment.systemPackages = builtins.attrValues {
    inherit (pkgs)
      git
      tig
      jujutsu
      lazygit
      ;
  };
  programs.git.config.init.defaultBranch = "main";
}
