extern crate rust_scripts;
use rust_scripts::hotkeys::*;

fn main() {
    let state;
    let location;

    let volume_up = (
        c("Volume up"),
        Fork(cmd(
            vec!["pactl", "set-sink-volume", "@DEFAULT_SINK@", "+5%"],
        )),
    );
    let volume_down = (
        c("Volume down"),
        Fork(cmd(
            vec!["pactl", "set-sink-volume", "@DEFAULT_SINK@", "-5%"],
        )),
    );
    let ncmpcpp = (c("Music"), Exec(cmd(vec!["urxvt", "-e", "ncmpcpp"])));
    let wizard = (c("Magic"), Script);
    let startmenu = Menu((c("Hauptmenü"), vec![wizard]))
    // arandr?
    // keymap
    // shutdown options
    // lautstärken micro, lautsprecher, regler zeigen
    // zeige Buchhaltung
    // mumble
    // hub.w17.io
    // select wifi
    // debug network
    // grammofy
    // browser + pm
    // IM dienste
    // evolution
    // door options, music options, hub options
    // lock inhibit
    // suspend inhibit
    // ssh kiva, ag, whisky, vorstand, shells, charon
    let w17menu 
    main_loop(startmenu);
    // Brightness -> i3
    // Basic Sound -> i3
}

fn wizard() -> Next {
    // states: work, research, normal, idle
    // enter new tasks
    // always: add task/subtask
    // what about running tasks?
    // check unread E-Mail + ak?
    // update
    //   system
    //   home
    // inbox
    //   
    // treesort
    // mails
    //   sort inbox
    //   sort inboxkiva
    //   sort inboxak
    //   sort to sort
    //   go trough todo
    //   go trough toread
    //   go trough readlater
    // pick tasks
    // optional, later
    // await
    // do accounting own, cda
    // dirty gits

    // task generator
    // task completion helper
}
