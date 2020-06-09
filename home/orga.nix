{ pkgs, config, ... }: {
  home.packages = [
    (pkgs.writeShellScriptBin "kassandra"
      ''exec ${config.home.homeDirectory}/.cargo/bin/kassandra "$@"'')
  ];
}
