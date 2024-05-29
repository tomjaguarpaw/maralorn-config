{ pkgs, ... }:
{
  environment.systemPackages = builtins.attrValues {
    inherit (pkgs)
      git
      tig
      jujutsu
      lazygit
      merge-bot
      ;
  };
  programs.git.config.init.defaultBranch = "main";
}
