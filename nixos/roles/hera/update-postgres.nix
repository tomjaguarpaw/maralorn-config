{ config, pkgs, ... }:
{
  environment.systemPackages =
    let
      # script from:
      # https://nixos.org/manual/nixos/stable/index.html#module-postgresql
      # 1) bump toVersion; rebuild
      # 2) run this script
      # 3) bump postgres; rebuild
      # 4) run analyze_new_cluster.sh; delete_old_cluster.sh from the database dir.
      toVersion = "15"; # Bump this when there is a new version.
      new_package = "postgresql_${toVersion}";
    in
    [
      (pkgs.writeScriptBin "upgrade-pg-cluster" ''
        set -eux

        systemctl stop postgresql

        export NEWDATA="/var/lib/postgresql/${toVersion}"

        export NEWBIN="${pkgs.${new_package}}/bin"

        export OLDDATA="${config.services.postgresql.dataDir}"
        export OLDBIN="${config.services.postgresql.package}/bin"

        install -d -m 0700 -o postgres -g postgres "$NEWDATA"
        cd "$NEWDATA"
        sudo -u postgres $NEWBIN/initdb -D "$NEWDATA"

        sudo -u postgres $NEWBIN/pg_upgrade \
          --old-datadir "$OLDDATA" --new-datadir "$NEWDATA" \
          --old-bindir $OLDBIN --new-bindir $NEWBIN \
          "$@"
      '')
    ];
}
