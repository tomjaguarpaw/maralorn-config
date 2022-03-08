{
  pkgs,
  config,
  ...
}: {
  home.file.".arbtt/categorize.cfg".source = pkgs.privateValue (builtins.toFile "empty-file" "") "arbtt/default";
}
