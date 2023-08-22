{
  pkgs,
  config,
  lib,
  ...
}:
let
  pythia-path = "${config.home.homeDirectory}/documents/pythia";
  pythia = pkgs.writeShellScriptBin "pythia" ''
    today=$(date +%Y-%m-%d)
    datei=${config.home.homeDirectory}/git/zettelkasten/pythia-$today.md
    if [[ ! -a $datei ]]; then
      neuron new --id pythia-$today Tagebucheintrag
      sed -i 's/date:/tags:\n - Pythia\ndate:/' $datei
    fi
    hx $datei
    grep TODO: $datei | sed 's/TODO: //' | while read -r line; do task add "$line"; done && sed -i 's/TODO: /Notiert: /' $datei
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
    ${lib.getExe pkgs.fortune} | ${run-printslow}
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
  run-pythia = lib.getExe pythia;
  run-printslow = lib.getExe printslow;
  hold = "echo -n '>'; read a;";
  dong = "${lib.getExe pkgs.mpv} dong.ogg &> /dev/null &";
  meditate = pkgs.writeShellScriptBin "meditate" ''
    cd ${pythia-path}
    start=`${lib.getExe pkgs.taskwarrior} calc now`
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
    ${lib.getExe pkgs.taskwarrior} calc now-$start
    ${lib.getExe pkgs.taskwarrior} gen_id:meditation done
    ${hold}
    exit
  '';
in
{
  home.packages = [
    pythia
    meditate
  ];
}
