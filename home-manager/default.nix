{ pkgs, config, ... }:
let
  me = config.m-0.private.me;
  meWork = config.m-0.private.meWork;
  unstablePkgs = import <unstable> {};
in {

imports = [
  ./zsh
  ./modules/taskwarrior.nix
  ./modules/force-copies.nix
  ./modules/battery.nix
  ./modules/laptop.nix
  ./modules/accounting.nix
  ./modules/rustdev.nix
  ./modules/latex.nix
  ./modules/sleep-nag.nix
  ./modules/graphical
  ./modules/mail.nix
  ./modules/home-options.nix
  ./modules/eventd.nix
  ./modules/unlock.nix
  ./modules/weechat
  ./modules/update_tasks.nix
  ../common
#   ./sort-mail.nix
#   ./morgenreport.nix
];

nixpkgs.config.packageOverrides = pkgs: with pkgs; {
  tasktree = callPackage ./packages/tasktree {};
  habitask = unstablePkgs.callPackage ./packages/habitask {};
  rust_scripts = unstablePkgs.callPackage ./packages/rust-scripts {};
  jali = callPackage ./packages/jali {};
  eventd = unstablePkgs.callPackage ./packages/eventd {};
  st = (import packages/st) pkgs config.m-0.colors;
  neovim = (import ./nvim) pkgs config.m-0.rustdev.enable;
};


home.file.".tmux.conf".text = ''
  set -g default-terminal "st-256color"
  set -ga terminal-overrides ",st-256color:Tc"
  set -g history-limit 50000
  set -g status off
  set -g escape-time 1
'';

programs = {
  git = {
    enable = true;
    ignores = [
      ".syncthing*.tmp"
      "*.swp"
      "*.autosave~"
      "*.aux"
      "*.bbl"
      "*.fls"
      "*.idx"
      "*.ilg"
      "*.ind"
      "*.log"
      "*.out"
      "*.toc"
      "*.bcf"
      "*.blg"
      "*.fdb*"
      "*.thm"
      "*.run.xml"
      "*.slnc"
      "*.glade~"
      "__pycache__"
      ".hledger-web_client_session_key.aes"
      ".nix-gc-roots"
    ];
    userEmail = me.mail;
    userName = me.name;
  };
  htop = {
    enable = true;
    hideThreads = true;
    hideUserlandThreads = true;
    highlightBaseName = true;
    shadowOtherUsers = true;
    showProgramPath = false;
    treeView = true;
  };
  ssh = {
    controlMaster = "yes";
    enable = true;
    matchBlocks = let
        matheGwProxy =  "ssh -q gw nc -q0 %h %p";
        kivaHost = "fb04386.mathematik.tu-darmstadt.de";
        agHost = "fb04217.mathematik.tu-darmstadt.de";
      in [
        { host = "charon"; hostname = "charon.olymp.space"; }
        { host = "hera"; hostname = "hera.m-0.eu"; }
        { host = "*.olymp.space"; user = me.user; }
        { host = "ag-forward"; hostname = agHost; proxyCommand = matheGwProxy; user = meWork.user; }
        { host = "ag"; hostname = agHost; user = meWork.user; }
        { host = "kiva-forward"; hostname = kivaHost; proxyCommand = matheGwProxy; user = meWork.user; }
        { host = "kiva"; hostname = kivaHost; user = meWork.user; }
        { host = "gw"; hostname = "gwres4.mathematik.tu-darmstadt.de"; user = meWork.user; }
        { host = "shells"; hostname = "shells.darmstadt.ccc.de"; }
        { host = "vorstand"; hostname = "vorstand.darmstadt.ccc.de"; }
        { host = "*.darmstadt.ccc.de"; user = me.user; }
        { host = "whisky"; hostname = "whisky.w17.io"; user = "chaos"; }
        { host = "kitchen"; hostname = "kitchen.w17.io"; user = "chaos"; }
        { host = "door.w17.io"; identityFile = "~/.ssh/door_rsa";}
      ];
  };
};

home.sessionVariables = {
  BROWSER = "${pkgs.firefox}/bin/firefox";
  EDITOR = "${pkgs.neovim}/bin/nvim";
  TERMINAL = config.m-0.terminal;
  EMAIL = me.mail;
  SUDO_ASKPASS = let
       print-pw = pkgs.writeShellScriptBin "print-pw" "pass eu/m-0/${config.m-0.hostName}";
    in
      "${print-pw}/bin/print-pw";
};

systemd.user.startServices = true;

home.packages = with pkgs; [
  neovim
];
xdg.enable = true;
}
