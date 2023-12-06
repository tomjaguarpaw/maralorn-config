{lib, config, ...}:
{
  services.swayidle = {
    events = [
      {
        event = "before-sleep";
        command = "${lib.getExe config.programs.swaylock.package} -f";
      }
    ];
    timeouts = [
      {
        timeout = 300;
        command = "${lib.getExe config.programs.swaylock.package} -f";
      }
    ];
  };
}
