{ pkgs, config, ... }:
let
  inherit (config.m-0.private) me meWork;
in {

imports = [
  ./zsh
  ./modules/taskwarrior.nix
  ./modules/force-copies.nix
  ./modules/battery.nix
  ./modules/laptop.nix
  ./modules/accounting
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
  ./modules/bugwarrior.nix
  ./modules/pythia.nix
  ../common
];

nixpkgs.overlays = [ (self: super: {
  tasktree = super.callPackage ./packages/tasktree {};
  jali = super.callPackage ./packages/jali {};
  eventd = super.callPackage ./packages/eventd {};
  neovim = (import ./nvim) super config.m-0.rustdev.enable;
})];

nixpkgs.config = {
  allowUnfree = true;
};


programs = {
  direnv = {
    enable = true;
    enableZshIntegration = true;
  };
  tmux = {
    enable = true;
    extraConfig = ''
      set default-terminal "screen-256color"
      set -g set-titles on
      set -g status off
      set -g escape-time 1
    '';
  };
  git = {
    aliases = {
      sync = "!git pull -r && git push";
    };
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
    controlMaster = "auto";
    controlPersist = "120";
    enable = true;
    matchBlocks = let
        matheGwProxy =  "ssh -q gw nc -q0 %h %p";
        agHost = "fb04217.mathematik.tu-darmstadt.de";
      in [
        { host = "charon"; hostname = "charon.olymp.space"; }
        { host = "hera"; hostname = "hera.m-0.eu"; forwardAgent = true; }
        { host = "ag-forward"; hostname = agHost; proxyCommand = matheGwProxy; user = meWork.user; }
        { host = "ag"; hostname = agHost; user = meWork.user; }
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
  PATH = "$HOME/.cargo/bin:/etc/profiles/per-user/${config.home.username}/bin:$HOME/.nix-profile/bin:$PATH";
  BROWSER = "${pkgs.firefox}/bin/firefox";
  EDITOR = "${pkgs.neovim}/bin/nvim";
  TERMINAL = config.m-0.terminal;
  EMAIL = me.mail;
  SUDO_ASKPASS = let
       print-pw = pkgs.writeShellScriptBin "print-pw" "pass show eu/m-0/${config.m-0.hostName}/user/${config.home.username}";
    in
      "${print-pw}/bin/print-pw";
};
fonts.fontconfig.enableProfileFonts = true;

systemd.user.startServices = true;

services = {
  gpg-agent = {
    enable = true;
    defaultCacheTtl = 31536000; # 1year
    maxCacheTtl = 31536000; #1year
  };
};

home.packages = with pkgs; [
  neovim
  (pkgs.writeShellScriptBin "print215" ''
    scp "$@" ag-forward:
    ssh ag-forward lpr -Zduplex -r "$@"
  '')
  (pkgs.writeShellScriptBin "print215single" ''
    scp "$@" ag-forward:
    ssh ag-forward lpr -r "$@"
  '')
];
xdg.enable = true;
}
