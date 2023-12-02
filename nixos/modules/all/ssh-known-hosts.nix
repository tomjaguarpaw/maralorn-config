{lib, config, ...}:
{
  programs.ssh.knownHosts =
    lib.mapAttrs (_: aliases: {extraHostNames = map (alias: "${alias}.maralorn.de") aliases;})
      config.m-0.hosts.aliases;
}
