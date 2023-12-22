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
    set-timer = pkgs.writeShellScriptBin "set-timer" ''
      if ! ${lib.getExe pkgs.jq} . ~/.timers &> /dev/null; then
        echo [] > ~/.timers
      fi
      if [[ "$2" == "" ]]; then
        new_timers=`${lib.getExe pkgs.jq} "map(select(.name != \"$1\"))" ~/.timers`
      else
        new_timers=`${lib.getExe pkgs.jq} "map(select(.name != \"$1\")) + [{name: \"$1\", at:$(${lib.getBin pkgs.coreutils}/bin/date +%s -d "$2")}]" ~/.timers`
      fi
      echo "$new_timers" > ~/.timers
    '';
    archive-nix-path =
      pkgs.writeHaskellScript
        {
          name = "archive-nix-path";
          bins = [
            # nix-copy-closure is buggy in nix 2.15
            pkgs.nixVersions.nix_2_13
            pkgs.openssh
          ];
          imports = [
            "Data.Time qualified as Time"
            "Data.List qualified as List"
          ];
        }
        ''
          main = do
            links <- getArgs <&> \case
               [] -> ["./result"]
               x -> x
            ${get_hostname}
            say "Collecting paths to upload"
            paths :: [String] <- fmap decodeUtf8 <$> (nix "path-info" links |> captureLines)
            when (null paths) do
              say "Found no paths to upload."
              exitFailure
            when (hostname /= "athene") do
              say [i|Uploading the following paths to athene:\n  #{List.intercalate "\n  " paths}|]
              nix "copy" "--to" "ssh://athene" paths --derivation
              nix "copy" "--to" "ssh://athene" paths
            now <- Time.getZonedTime
            let timestamp =  Time.formatTime Time.defaultTimeLocale "%F-%T" now
            paths & mapM_ \path -> do
              let name = drop 44 path
                  gc_root_full = [i|/disk/volatile/nix-gc-roots/#{timestamp}-#{name}|] :: String
                  gc_root = [i|/disk/volatile/nix-gc-roots/#{name}|] :: String
              say [i|Setting gc-roots #{gc_root} and #{gc_root_full}|]
              ssh "athene" "nix" "build" "-o" gc_root_full path
              ssh "athene" "nix" "build" "-o" gc_root path
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
      main = do
        ${get_mode}
        say [i|Switching to mode #{mode}...|]
        exe ([i|${modeDir}/#{mode}/activate|] :: String)
    '';
    selectMode =
      pkgs.writeHaskellScript
        {
          name = "select-mode";
          bins = [
            pkgs.river
            pkgs.activateMode
          ];
          imports = [ "System.Directory qualified as Directory" ];
        }
        ''
          main = do
            [mode] <- getArgs
            writeFile "${modeFile}" mode
            activate_mode
            riverctl "exit"
        '';
    updateModes =
      pkgs.writeHaskellScript
        {
          name = "update-modes";
          bins = [
            pkgs.activateMode
            pkgs.git
            pkgs.nix-output-monitor
            pkgs.builders-configurator
            pkgs.archive-nix-path
          ];
        }
        ''
          main = do
            ${get_hostname}
            ${get_builders}
            say [i|Building modes for #{hostname} …|]
            nom ["build", "--builders", [i|@#{builders}|], [i|${configPath}\#homeModes.#{hostname}|], "-o", "${modeDir}"]
            activate_mode
            archive_nix_path "${modeDir}"
        '';
    quickUpdateMode =
      pkgs.writeHaskellScript
        {
          name = "quick-update-mode";
          bins = [
            pkgs.updateModes
            pkgs.git
            pkgs.home-manager
            pkgs.nix-output-monitor
            pkgs.builders-configurator
          ];
        }
        ''

          main = do
            ${get_hostname}
            ${get_mode}
            ${get_builders}
            say [i|Quick switching to mode #{mode} ...|]
            path <- decodeUtf8 @Text <$> (nom ["build", "--builders", [i|@#{builders}|], "--print-out-paths", "--no-link", [i|${configPath}\#homeConfigurations.#{hostname}-#{mode}.activationPackage|]] |> captureTrim)
            exe ([i|#{path}/activate|] :: String)
            update_modes
        '';
    updateSystem =
      pkgs.writeHaskellScript
        {
          name = "update-system";
          bins = [
            # nix-copy-closure is buggy in nix 2.15
            pkgs.nixVersions.nix_2_13
            pkgs.openssh
            pkgs.archive-nix-path
            pkgs.builders-configurator
            pkgs.nix-output-monitor
            pkgs.iputils
          ];
        }
        ''
          deploy :: Maybe String -> IO ()
          deploy remote_host = do
            ${get_hostname}
            let host = fromMaybe (decodeUtf8 hostname) remote_host
                is_remote = host /= decodeUtf8 hostname
            say [i|Building configuration for #{host} …|]
            ${get_builders}
            path <- decodeUtf8 @String <$> (nom ["build", "--builders", [i|@#{builders}|], "--print-out-paths", "--no-link", [i|${configPath}\#nixosConfigurations.#{host}.config.system.build.toplevel|]] |> captureTrim)
            archive_nix_path path
            on_target <- is_remote & \case
              True -> do
                say [i|Uploading derivation to #{host} …|]
                nix "copy" "--derivation" path "--to" ([i|ssh://#{host}|] :: String)
                say [i|Uploading configuration to #{host} …|]
                nix "copy" path "--to" ([i|ssh://#{host}|] :: String)
                pure (ssh ([i|root@#{host}|] :: String))
              False -> pure (exe "/run/wrappers/bin/sudo")
            on_target ["nix-env", "-p", "/nix/var/nix/profiles/system", "--set", path]
            on_target [[i|#{path}/bin/switch-to-configuration|], "switch"]

          is_reachable host = do
            ping_result <- tryFailure (ping "-W0.5" "-c1" host &> devNull)
            ping_result & \case
              Left _ -> say [i|#{host} not reachable. skipping.|] >> pure False
              Right _ -> pure True

          main = getArgs >>= \case
            [] -> deploy Nothing
            hosts -> (if hosts == ["all"] then ["apollo", "hephaistos", "zeus", "athene", "hera"] else hosts)
              & filterM is_reachable
              >>= mapM_ (deploy . Just)
        '';
  };
in
mode-scripts // { inherit mode-scripts; }
