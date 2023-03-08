final: _: let
  inherit (final) pkgs lib;
  homeDir = "/home/maralorn";
  modeFile = "${homeDir}/.volatile/mode";
  modeDir = "${homeDir}/.volatile/modes";
  configPath = "${homeDir}/git/config";
  configGit = "${lib.getExe pkgs.git} -C ${configPath}";
  get_mode = ''
    mode <- decodeUtf8 @Text <$> (cat "${modeFile}" |> captureTrim)
  '';
  get_available_modes = ''
    available_modes <- Directory.listDirectory "${modeDir}"
  '';
  get_hostname = ''
    hostname <- BSC.strip <$> readFileBS "/etc/hostname"
  '';
  get_builders = ''
    builders <- builders_configurator |> captureTrim
  '';
  mode-scripts = {
    maintenance = pkgs.writeShellScriptBin "maintenance" ''
      set -e
      ${configGit} pull --ff-only
      echo "Running update-modes …"
      ${lib.getExe pkgs.updateModes}
      echo "Updating system …"
      ${lib.getExe pkgs.updateSystem}
      echo "Maintenance finished."
    '';
    activateMode = pkgs.writeHaskellScript {name = "activate-mode";} ''
      wallpaperCmd = "random-wallpaper"

      main = do
        ${get_mode}
        say [i|Switching to mode #{mode}...|]
        exe ([i|${modeDir}/#{mode}/activate|] :: String)
        whenM (elem wallpaperCmd <$> pathBins) $ exe wallpaperCmd
    '';
    selectMode =
      pkgs.writeHaskellScript
      {
        name = "select-mode";
        bins = [
          pkgs.activateMode
          pkgs.psmisc
        ];
        imports = ["System.Directory qualified as Directory"];
      } ''
        main = do
          [mode] <- getArgs
          ${get_available_modes}
          unless (mode `elem` available_modes) do say [i|"#{mode}" is not a known mode|]; exitFailure
          writeFile "${modeFile}" mode
          activate_mode
          ignoreFailure $ killall ["GeckoMain", "firefox", ".firefox-wrapped"]
      '';
    updateModes =
      pkgs.writeHaskellScript
      {
        name = "update-modes";
        bins = [pkgs.activateMode pkgs.git pkgs.nix-output-monitor pkgs.builders-configurator];
      } ''
        main = do
          ${get_hostname}
          say [i|Building ~/.modes for #{hostname}|]
          ${get_builders}
          nom ["build", "--builders", [i|@#{builders}|], [i|${configPath}\#homeModes.#{hostname}|], "-o", "${modeDir}"]
          activate_mode
      '';
    quickUpdateMode =
      pkgs.writeHaskellScript
      {
        name = "quick-update-mode";
        bins = [pkgs.updateModes pkgs.git pkgs.home-manager pkgs.nix-output-monitor pkgs.builders-configurator];
      } ''

        main = do
          ${get_hostname}
          ${get_mode}
          ${get_builders}
          say [i|Quick switching to mode #{mode} ...|]
          path <- decodeUtf8 @Text <$> (nom ["build", "--builders", [i|@#{builders}|], "--print-out-paths", "--no-link", [i|${configPath}\#homeConfigurations.#{hostname}-#{mode}.activationPackage|]] |> captureTrim)
          exe ([i|#{path}/activate|] :: String)
          update_modes
      '';
    updateSystem = pkgs.writeShellScriptBin "update-system" ''
      set -e
      remote_host=$1
      host=''${remote_host:-$(hostname)}
      echo "Building configuration for $host …"
      output=$(nom build --builders @$(builders-configurator) /home/maralorn/git/config#nixosConfigurations.$host.config.system.build.toplevel --no-link --print-out-paths)
      if [[ -z "$remote_host" ]]; then
        on_target() {
          /run/wrappers/bin/sudo $@
        }
      else
        on_target() {
          ${pkgs.lib.getExe pkgs.openssh} root@$host $@
        }
        echo "Uploading configuration to $host …"
        ${final.lib.getExe pkgs.nix} copy $output --to ssh://$host
      fi
      on_target ${pkgs.nix}/bin/nix-env -p /nix/var/nix/profiles/system --set $output
      on_target $output/bin/switch-to-configuration switch
    '';
  };
in
  mode-scripts
  // {
    inherit mode-scripts;
  }
