final: _:
let
  inherit (final) pkgs lib;
  homeDir = "/home/maralorn";
  modeFile = "${homeDir}/.mode";
  modeDir = "${homeDir}/.volatile/modes";
  configPath = "${homeDir}/git/config";
  configGit = "${lib.getExe pkgs.git} -C ${configPath}";
  get_mode = ''
    mode <- decodeUtf8 @Text <$> (cat "${modeFile}" |> captureTrim)
  '';
  get_hostname = ''
    hostname <- BSC.strip <$> readFileBS "/etc/hostname"
  '';
  get_builders = ''
    builders <- builders_configurator |> captureTrim
  '';
  mode-scripts = {
    archive-nix-path = pkgs.writeHaskellScript {
      name = "archive-nix-path";
      bins = [ pkgs.nix pkgs.openssh ];
      imports = [ "Data.Time qualified as Time" "Data.List qualified as List" ];
    } ''
      main = do
        links <- getArgs
        when (null links) do
          say "Usage: archive-nix-path <tag> <installables…>"
          exitFailure
        say "Collecting paths to upload"
        paths :: [String] <- fmap decodeUtf8 <$> (nix "path-info" links |> captureLines)
        when (null paths) do
          say "Found no paths to upload."
          exitFailure
        say [i|Uploading the following paths to fluffy:\n  #{List.intercalate "\n  " paths}|]
        nix_copy_closure "--to" "fluffy" paths |!> readInputLines (mapM_ $ BS.toStrict <&> \case
          line | BSC.isInfixOf " from " line -> BS.putStr "c"
          line | BSC.isInfixOf " to " line -> BS.putStr "u"
          line -> BSC.putStrLn line)
        BSC.putStrLn ""
        now <- Time.getZonedTime
        let timestamp =  Time.formatTime Time.defaultTimeLocale "%F-%T" now
        paths & mapM_ \path -> do
            let gc_root = [i|/disk/volatile/nix-gc-roots/#{drop 44 path}-#{timestamp}|] :: String
            say [i|Setting gc-root #{gc_root}|]
            ssh "fluffy" "nix" "build" "-o" gc_root path
    '';
    maintenance = pkgs.writeShellScriptBin "maintenance" ''
      set -e
      ${configGit} pull --ff-only
      echo "Running update-modes …"
      ${lib.getExe pkgs.updateModes}
      echo "Updating system …"
      ${lib.getExe pkgs.updateSystem}
      echo "Maintenance finished."
    '';
    activateMode = pkgs.writeHaskellScript { name = "activate-mode"; } ''
      wallpaperCmd = "random-wallpaper"

      main = do
        ${get_mode}
        say [i|Switching to mode #{mode}...|]
        exe ([i|${modeDir}/#{mode}/activate|] :: String)
        whenM (elem wallpaperCmd <$> pathBins) $ exe wallpaperCmd
    '';
    selectMode = pkgs.writeHaskellScript {
      name = "select-mode";
      bins = [ pkgs.activateMode pkgs.psmisc ];
      imports = [ "System.Directory qualified as Directory" ];
    } ''
      main = do
        [mode] <- getArgs
        writeFile "${modeFile}" mode
        activate_mode
        ignoreFailure $ killall ["GeckoMain", "firefox", ".firefox-wrapped",".electron-wrapped","signal-desktop"]
    '';
    updateModes = pkgs.writeHaskellScript {
      name = "update-modes";
      bins = [
        pkgs.activateMode
        pkgs.git
        pkgs.nix-output-monitor
        pkgs.builders-configurator
        pkgs.archive-nix-path
      ];
    } ''
      main = do
        ${get_hostname}
        ${get_builders}
        say [i|Building modes for #{hostname} …|]
        nom ["build", "--builders", [i|@#{builders}|], [i|${configPath}\#homeModes.#{hostname}|], "-o", "${modeDir}"]
        activate_mode
        archive_nix_path "${modeDir}"
    '';
    quickUpdateMode = pkgs.writeHaskellScript {
      name = "quick-update-mode";
      bins = [
        pkgs.updateModes
        pkgs.git
        pkgs.home-manager
        pkgs.nix-output-monitor
        pkgs.builders-configurator
      ];
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
      output=$(nom build --builders @$(builders-configurator) $HOME/git/config#nixosConfigurations.$host.config.system.build.toplevel --no-link --print-out-paths)
      if [[ -z "$remote_host" ]]; then
        on_target() {
          /run/wrappers/bin/sudo -A $@
        }
      else
        on_target() {
          ${pkgs.lib.getExe pkgs.openssh} root@$host $@
        }
        echo "Uploading derivation to $host …"
        ${final.lib.getExe pkgs.nix} copy --derivation $output --to ssh://$host
        echo "Uploading configuration to $host …"
        ${final.lib.getExe pkgs.nix} copy $output --to ssh://$host
      fi
      on_target ${pkgs.nix}/bin/nix-env -p /nix/var/nix/profiles/system --set $output
      on_target $output/bin/switch-to-configuration switch
      archive-nix-path $output
    '';
  };
in mode-scripts // { inherit mode-scripts; }
