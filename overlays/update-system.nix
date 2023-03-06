final: _: {
  updateSystem = final.writeShellScriptBin "update-system" ''
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
        ${final.lib.getExe final.openssh} root@$host $@
      }
      echo "Uploading configuration to $host …"
      ${final.lib.getExe final.nix} copy $output --to ssh://$host
    fi
    on_target ${final.nix}/bin/nix-env -p /nix/var/nix/profiles/system --set $output
    on_target $output/bin/switch-to-configuration switch
  '';
}
