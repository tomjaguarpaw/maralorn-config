extern crate rust_scripts;
extern crate task_hookrs;
#[macro_use]
extern crate error_chain;

use std::rc::Rc;
use rust_scripts::hotkeys::{run, run_cmd, term, menu, main_loop, Next};
use rust_scripts::kassandra::{kassandra, change_state, new_tasks};
use rust_scripts::error::Result;

quick_main!(main_function);

fn main_function() -> Result<()> {
    let w17menu = {
        let summer = term("Summer", "ssh summer@door.w17.io");
        let lock = term("Lock", "ssh close@door.w17.io");
        let unlock = term("Unlock", "ssh open@door.w17.io");
        let mpd_whisky = term("MPD Whisky", "ncmpcpp -h whisky");
        let hub = run("Hub", "firefox --new-window https://hub.w17.io");
        let kitchen = run("Kitchen", "firefox --new-window http://kitchen.w17.io");
        menu("w17", vec![lock, unlock, summer, hub, mpd_whisky, kitchen])
    };

    let powermenu = {
        let inhibit = term(
            "Inhibit Suspend",
            "systemd-inhibit --what handle-lid-switch watch echo 'Lid switch inhibited'",
        );
        let logout = run("Logout", "i3-msg exit");
        let shutdown = run("Shutdown", "systemctl poweroff");
        let suspend = run("Suspend", "systemctl suspend");
        let reboot = run("Reboot", "systemctl reboot");
        let lock = run("Lock", "loginctl lock-session");
        menu(
            "Power",
            vec![shutdown, suspend, reboot, lock, inhibit, logout],
        )
    };
    let soundmenu = {
        let mpd = term("MPD", "ncmpcpp");
        let grammofy = run("Grammofy", "firefox --new-window https://grammofy.com");
        let pavucontrol = run("Lautstärke", "pavucontrol");
        menu("Sound", vec![mpd, pavucontrol, grammofy])
    };
    let apps = {
        menu(
            "Apps",
            vec![
                run("Launch", "rofi -show combi"),
                run("Private Browser", "firefox --private-window"),
                run("Browser", "firefox --new-window"),
                run("Deluge", "deluge"),
                run("Filemanager", "nautilus"),
                menu(
                    "Messaging",
                    vec![
                        run("Mails", "evolution"),
                        term("Mutt", "neomutt"),
                        run_cmd(
                            "Weechat",
                            vec![
                                "st".into(),
                                "-e".into(),
                                "ssh".into(),
                                "-t".into(),
                                "hera".into(),
                                "tmux -L weechat attach".into(),
                            ]
                        ),
                        run("Riot", "firefox --new-window https://riot.im/app"),
                        run("Mastodon", "firefox --new-window https://chaos.social"),
                        run("Telegram", "telegram-desktop"),
                        run("Signal", "signal-desktop"),
                        run("Jabber", "dino"),
                        run(
                            "Regiotelko",
                            "mumble mumble://maralorn@mumble.c3pb.de/CCC/Regiotelko"
                        ),
                    ]
                ),
                menu(
                    "Accounting",
                    vec![
                        term("Jali", "jali -l ."),
                        run(
                            "Beschlüsse",
                            "firefox --new-window https://git.darmstadt.ccc.de/vorstand/beschluesse/raw/master/beschl%C3%BCsse"
                        ),
                        term(
                            "Private Buchhaltung",
                            "hledger -f ~/git/buchhaltung/buchhaltung.ledger ui"
                        ),
                        term(
                            "CDA Buchhaltung",
                            "hledger -f ~/git/cda/buchhaltung/buchhaltung.ledger ui"
                        ),
                    ]
                ),
            ],
        )
    };
    let maintenance = {
        let keymenu = menu(
            "Keymap",
            vec![
                run("neo", "setxkbmap de neo"),
                run("qwertz", "setxkbmap de"),
                run("qwerty", "setxkbmap us"),
            ],
        );
        let monitor = term("Monitor", "htop");
        let wifi = term("WLAN", "nmtui");
        menu(
            "Maintenance",
            vec![wifi, monitor, keymenu, run("Bildschirme", "arandr")],
        )
    };
    let ssh = menu(
        "ssh",
        vec![
            ("kiva", "brandy@kiva-forward"),
            ("ag", "brandy@ag-forward"),
            ("whisky", "whisky"),
            ("kitchen", "kitchen"),
            ("vorstand", "vorstand"),
            ("shells", "shells"),
            ("hera", "hera"),
            ("charon", "charon"),
        ].into_iter()
            .map(|(name, login)| term(name, &format!("ssh {}", login)))
            .collect(),
    );

    // sshuttle
    // tinc
    // routes
    // debug network
    // lock inhibit
    let startmenu = menu(
        "Hauptmenü",
        vec![
            (
                "New Task".into(),
                Next::Do(Rc::new(|| new_tasks().map(|_| Next::Exit)))
            ),
            (
                "Kassandra".into(),
                Next::Do(Rc::new(|| kassandra().map(|_| Next::Exit)))
            ),
            (
                "Change State".into(),
                Next::Do(Rc::new(|| change_state().map(|_| Next::Exit)))
            ),
            run("Habitica", "firefox --new-window https://habitica.com"),
            ssh,
            apps,
            run("Tasks", "tasklauncher"),
            run("Tasktree", "tasktree"),
            term("Meditate", "meditate"),
            term("Pythia", "pythia"),
            term("Files", "ranger"),
            soundmenu,
            w17menu,
            powermenu,
            maintenance,
        ],
    ).1;
    main_loop(startmenu)
}
