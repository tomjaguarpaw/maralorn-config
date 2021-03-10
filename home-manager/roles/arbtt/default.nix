{ pkgs, config, ... }: {
  home.file.".arbtt/categorize.cfg".source = pkgs.privateFile "arbtt/categorize.cfg";
}
