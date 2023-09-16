{
  pkgs,
  lib,
  config,
  ...
}:
let
  lock-command = "${lib.getExe config.programs.swaylock.package} -f";
in
{
  services.swayidle = {
    # Here we override the before-sleep eveent
    # swayidle only runs the last before-sleep command given on the commandline
    events = lib.mkAfter [ {
      event = "before-sleep";
      command =
        (pkgs.writeShellScript "total-mute" ''
          echo "Running total mute";
          ${lib.getExe pkgs.mpc-cli} pause
          for ((index=0; index < $(${pkgs.pulseaudio}/bin/pactl list short sinks | ${
            lib.getBin pkgs.coreutils
          }/bin/wc -l); index++)); do
            ${pkgs.pulseaudio}/bin/pactl set-sink-mute $index 1;
          done
          for ((index=0; index < $(${pkgs.pulseaudio}/bin/pactl list short sources | ${
            lib.getBin pkgs.coreutils
          }/bin/wc -l); index++)); do
            ${pkgs.pulseaudio}/bin/pactl set-source-mute $index 1;
          done
          ${lock-command}
        '').outPath;
    } ];
    timeouts = [ {
      timeout = 290;
      command = lock-command;
    } ];
  };
}
