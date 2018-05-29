{ pkgs, config, ... }:
{
  nixpkgs.config.packageOverrides = pkgs: {
    rust-scripts = pkgs.callPackage ./packages/rust-scripts {};
    jali = pkgs.callPackage ./packages/jali {};
    eventd = (import <unstable> {}).callPackage ./packages/eventd {};
    st = (import graphical/st) pkgs config.common.colors;
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
    zsh = {
      enable = true;
      enableAutosuggestions = true;
      enableCompletion = true;
      history = {
        save = 100000;
        size = 100000;
      };
      initExtra = builtins.readFile ./configs/zshrc;
      oh-my-zsh = {
        enable = true;
        plugins = [ "colored-man-pages" "git-prompt" ];
      };
    };
  };

  home.sessionVariables = {
    BROWSER="${pkgs.firefox}/bin/firefox";
    EDITOR="${pkgs.neovim}/bin/nvim";
    TERMINAL=config.common.terminal;
  };
  systemd.user.startServices = true;

  imports = [
    ./taskwarrior.nix
    ../modules/force-copies.nix
  ];

  home.packages = with pkgs; [
    htop
    tree
    rxvt_unicode.terminfo
    most

    socat
    nmap
    tcpdump

    rcm
    tmux
    tig
    exa
    fzf
    ag

    pythonPackages.qrcode
    ranger

    (pkgs.neovim.override {
      vimAlias = true;
      withPython3 = true;
      configure = {
        customRC = ''
          let $RUST_SRC_PATH="${pkgs.rustPlatform.rustcSrc}"
          let g:rustfmt_command = "${pkgs.rustfmt}/bin/rustfmt"
          let g:racer_cmd = "${pkgs.rustracer}/bin/racer"
          let g:deoplete#sources#rust#racer_binary='${pkgs.rustracer}/bin/racer'
          let g:syntastic_rust_rustc_exe = '${pkgs.cargo}/bin/cargo check'
          ${builtins.readFile ./configs/vimrc}
       '';
        packages.myVimPackage = with pkgs.vimPlugins; {
          start = [
            vim-nix
            ctrlp
            vimtex
            Syntastic
            UltiSnips
            airline
            rust-vim
            fugitive
            airline
            vim-trailing-whitespace
            vim-polyglot
            nvim-cm-racer
            nvim-completion-manager
            vim-pandoc
            nerdcommenter
            vim-signify
          ];
        };
      };
    })
  ];
  xdg.enable = true;
}
