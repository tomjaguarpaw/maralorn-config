{ lib, pkgs, config, ...}:
with lib;
{

options.m-0.pythia.enable = mkEnableOption "Pythia";
config = mkIf config.m-0.pythia.enable (let
  pythia-path = "${config.home.homeDirectory}/data/aktuell/pythia";
  pythia = pkgs.writeShellScriptBin "pythia" ''
    datei=${pythia-path}/$(date +%Y-%m-%d)
    touch $datei
    vim $datei
    grep TODO: $datei | sed 's/TODO: //' | while read -r line; do task add "$line"; done && sed -i 's/TODO: /Notiert: /' $datei
    grep WEAK: $datei | sed 's/WEAK: //' >> ${pythia-path}/schwächen && sed -i 's/WEAK: /Schwäche: /' $datei
    grep STRONG: $datei | sed 's/STRONG: //' >> ${pythia-path}/stärken && sed -i 's/STRONG: /Stärke: /' $datei
    grep RULE: $datei | sed 's/RULE: //' >> ${pythia-path}/richtlinien && sed -i 's/RULE: /Richtlinie: /' $datei
    grep INDICATOR: $datei | sed 's/INDICATOR: //' >> ${pythia-path}/warnzeichen && sed -i 's/INDICATOR: /Warnzeichen: /' $datei
  '';
  printslow = pkgs.writeScriptBin "printslow" ''
    #!${pkgs.python3}/bin/python

    import fileinput
    import sys
    import time

    for line in fileinput.input():
        for letter in line:
            print(letter, end="")
            sys.stdout.flush()
            time.sleep(0.1)
    '';
  fortune = ''
    ${pkgs.fortune}/bin/fortune | ${run-printslow}
    echo
    sleep 5s
  '';
  threefortunes = ''
    echo Genieße drei Fortunes: | ${run-printslow}
    echo
    ${fortune}
    ${fortune}
    ${fortune}
  '';
  run-pythia = "${pythia}/bin/pythia";
  run-printslow = "${printslow}/bin/printslow";
  hold = "echo -n '>'; read a;";
  dong = "${pkgs.mpv}/bin/mpv dong.ogg &> /dev/null &";
  meditate = pkgs.writeShellScriptBin "meditate" ''
cd ${pythia-path}
i3-msg fullscreen &> /dev/null
start=`${pkgs.taskwarrior}/bin/task calc now`
mpv background.ogg &> /dev/null &
${run-printslow} << EOF
Hallo Malte,

hier spricht Pythia.

Herzlich willkommen zur Besinnung, Ruhefindung und Orientierung.

Bist Du bereit?
EOF
${hold}
${run-printslow} << EOF
Gut, dann lass uns starten.
EOF
sleep 3s
${run-printslow} << EOF

Nimm Dir ein wenig Zeit um die Gedanken zu sammeln, die unerledigt sind.

EOF
sleep 1s
${run-pythia}
${run-printslow} << EOF

Nimm Dir nun mindestens 5 Minuten Auszeit um tief zu entspannen.
Achte auf Deine Atmung und Deinen Körper.
Beruhige Deine Gedanken und gehe in Dich.

EOF
${dong}
sleep 5m
${dong}
${hold}
${threefortunes}
${run-printslow} << EOF
Nun ist die Zeit für Reflektion und um an Deinem Credo zu arbeiten.
EOF
sleep 3s
${run-pythia}
${run-printslow} << EOF
Melde Dich, wenn Du bereit für Dein Credo bist.
EOF
${hold}
${run-printslow} credo
${hold}
${run-printslow} << EOF

Nun genieße die gewonne Energie um Dich auf die Zukunft vorzubereiten.
EOF
${hold}
${threefortunes}
${dong}
echo Dauer der Meditation | ${run-printslow}
${pkgs.taskwarrior}/bin/task calc now-$start
${pkgs.taskwarrior}/bin/task gen_id:meditation done
${hold}
exit
    '';
    in {

  home.packages = with pkgs; [
      pythia
      meditate
  ];
});

}
