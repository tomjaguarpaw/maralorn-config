{ pkgs, ... }:
{
  environment.systemPackages = builtins.attrValues { inherit (pkgs) git tig lazygit; };
  programs.git.config.init.defaultBranch = "main";
}
