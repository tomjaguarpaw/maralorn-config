{ pkgs, config, ... }:
{
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
    ./modules/home-options.nix
#   ./sort-mail.nix
#   ./morgenreport.nix
#   ./my-systems.nix
  ];

  nixpkgs.config.packageOverrides = pkgs: with pkgs; {
    tasktree = callPackage ./packages/tasktree {};
    rust-scripts = callPackage ./packages/rust-scripts {};
    jali = with pkgs; callPackage ./packages/jali {};
    eventd = (import <unstable> {}).callPackage ./packages/eventd {};
    st = (import packages/st) pkgs config.common.colors;
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
    home-manager = {
      enable = true;
      path = https://github.com/rycee/home-manager/archive/master.tar.gz;
    };
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
      userEmail = "malte.brandy@maralorn.de";
      userName = "Malte Brandy";
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
          { host = "*.olymp.space"; user = "maralorn"; }
          { host = "ag-forward"; hostname = agHost; proxyCommand = matheGwProxy;user="brandy";}
          { host = "ag"; hostname = agHost;user="brandy";}
          { host = "kiva-forward"; hostname = kivaHost; proxyCommand = matheGwProxy;user="brandy";}
          { host = "kiva"; hostname = kivaHost;user="brandy";}
          { host = "gw"; hostname = "gwres4.mathematik.tu-darmstadt.de";user="brandy";}
          { host = "shells"; hostname = "shells.darmstadt.ccc.de"; }
          { host = "vorstand"; hostname = "vorstand.darmstadt.ccc.de"; }
          { host = "*.darmstadt.ccc.de"; user = "maralorn"; }
          { host = "whisky"; hostname = "whisky.w17.io"; user = "chaos"; }
          { host = "door.w17.io"; identityFile = "~/.ssh/door_rsa";}
        ];
    };
  };

  home.sessionVariables = {
    BROWSER="${pkgs.firefox}/bin/firefox";
    EDITOR="${pkgs.neovim}/bin/nvim";
    TERMINAL=config.common.terminal;
  };
  systemd.user.startServices = true;

  home.packages = with pkgs; [
    neovim
  ];
  xdg.enable = true;
}
