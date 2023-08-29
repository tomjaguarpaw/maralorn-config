{ pkgs, lib, ... }:
let
  command = "${lib.getExe pkgs.swaylock} -f";
in
{
  services.swayidle = {
    events = [
      {
        event = "before-sleep";
        inherit command;
      }
      {
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
          '').outPath;
      }
      {
        event = "lock";
        inherit command;
      }
    ];
    timeouts = [ {
      timeout = 300;
      inherit command;
    } ];
  };
}
