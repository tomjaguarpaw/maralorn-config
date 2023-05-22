{ config, ... }: {
  xdg.configFile."monitors.xml".source = ./.
    + "/${config.m-0.hostName}-monitors.xml";
}
