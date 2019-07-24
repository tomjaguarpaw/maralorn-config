{ pkgs, config, ... }:
let
  inherit (import ../common/my-lib.nix) writeHaskellScript getNivPath;
  sources = import ../nix/sources.nix;
  inherit (config.m-0.private) me meWork;
  configPath = "/home/${config.home.username}/git/nixos/config";
  gcRetentionDays = 5;
  update-home-manager = writeHaskellScript
    {
      name = "update-home-manager";
      imports = [
        "qualified Data.ByteString.Lazy.Char8 as C"
        "qualified Data.List as L"
      ];
      bins = [
        getNivPath
        (pkgs.callPackage <home-manager/home-manager> {})
      ];
    }
    ''

    getNivAssign name = fmap tag . readTrim $ get_niv_path "${configPath}/nix/sources.nix" name
      where tag str = ["-I", name ++ "=" ++ C.unpack str]

    main = do
      paths <- mapM getNivAssign ["home-manager", "nixpkgs", "unstable"]
      home_manager (concat paths ++ ["switch"])
    '';
  user-maintenance = writeHaskellScript
    { name = "user-maintenance"; imports = [ ]; bins = [ update-home-manager pkgs.nix pkgs.git];} ''
    main = do
      git "-C" "${configPath}" "pull"
      update_home_manager
      nix_collect_garbage "--delete-older-than" "${toString gcRetentionDays}"
      nix "optimise-store"
  '';
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

home.file = {
  home-manager-source = {
    target = ".nix-path/home-manager";
    source = sources.home-manager;
  };
};

programs = {
  home-manager.enable = true;
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
  NIX_PATH = "$HOME/.nix-path:$NIX_PATH";
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

home.packages = builtins.attrValues {
  inherit (pkgs) neovim;
  inherit update-home-manager user-maintenance;
  print215 = (pkgs.writeShellScriptBin "print215" ''
    scp "$@" ag-forward:
    ssh ag-forward lpr -Zduplex -r "$@"
  '');
  print215single = (pkgs.writeShellScriptBin "print215single" ''
    scp "$@" ag-forward:
    ssh ag-forward lpr -r "$@"
  '');
};
xdg.enable = true;
}
