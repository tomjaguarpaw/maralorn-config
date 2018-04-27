extern crate dialog;
extern crate task_hookrs;


pub mod hotkeys {
    use std::rc::Rc;
    use dialog::DialogProvider;
    use dialog::errors::Error;
    use dialog::errors::ErrorKind::*;
    use dialog::rofi::RofiDialogProvider;
    use std::process::Command as StdCommand;
    use std::os::unix::process::CommandExt;
    pub use hotkeys::Next::*;

    #[derive(Clone)]
    pub enum Next {
        Menu(Dialog),
        TmpMenu(Dialog),
        Run(Command),
        Fork(Command),
        Exec(Command),
        Back,
        Exit,
        Stay,
        Script(Rc<Fn(State) -> Next>),
    }

    type Dialog = (String, Vec<Item>);

    type Item = (String, Next);

    type Command = Vec<String>;

    type State = String;

    pub fn c<T>(string: T) -> String
    where
        T: Into<String>,
    {
        string.into()
    }

    pub fn cmd<S, K>(argv: S) -> Command
    where
        S: IntoIterator<Item = K>,
        K: Into<String>,
    {
        argv.into_iter().map(|x| x.into()).collect()
    }


    fn show_menu<T: DialogProvider>(dialog_provider: &mut T, menu: Dialog) -> Next {
        let (msg, options) = menu;
        match dialog_provider.select_option(msg, options) {
            Ok(next) => next,
            Err(Error(InputCanceled, _)) => Exit,
            Err(Error(InvalidUserInput, _)) => Stay,
            Err(err) => {
                println!("DialogError: {}", err);
                Stay
            }
        }
    }


    fn build_command(cmd: Command) -> StdCommand {
        let mut cmd_iter = cmd.iter();
        let mut prg = StdCommand::new(cmd_iter.next().expect("Called program without name"));
        for arg in cmd_iter {
            prg.arg(arg);
        }
        prg
    }

    pub fn main_loop(startmenu: Next) {
        let mut dialog_provider = RofiDialogProvider;
        let mut history = vec![];
        let mut next = startmenu;
        loop {
            next = match next {
                Exit => break,
                Back => {
                    history.pop().expect("Empty history");
                    Stay
                }
                Script(script) => script(c("work")),
                TmpMenu(menu) => show_menu(&mut dialog_provider, menu),
                Menu(menu) => {
                    history.push(menu.clone());
                    show_menu(&mut dialog_provider, menu)
                }
                Fork(cmd) => {
                    build_command(cmd).spawn().is_ok();
                    Stay
                }
                Run(cmd) => {
                    build_command(cmd).output().is_ok();
                    Stay
                }
                Exec(cmd) => {
                    build_command(cmd).exec();
                    break;
                }
                Stay => Menu(history.pop().expect("Empty history").clone()),
            };
        }
    }
}
