{ pkgs, ... }:
{
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
      controlMaster = "autoask";
      enable = true;
      matchBlocks = let
          matheGwProxy =  "ssh -q gw nc -q0 %h %p";
          kivaHost = "fb04386.mathematik.tu-darmstadt.de";
          agHost = "fb04217.mathematik.tu-darmstadt.de";
        in [
          { host = "charon"; hostname = "charon.olymp.space"; }
          { host = "*.olymp.space"; user = "maralorn"; }
          { host = "ag-forward"; hostname = agHost; proxyCommand = matheGwProxy; }
          { host = "ag"; hostname = agHost; }
          { host = "kiva-forward"; hostname = kivaHost; proxyCommand = matheGwProxy; }
          { host = "kiva"; hostname = kivaHost; }
          { host = "gw"; hostname = "gwres4.mathematik.tu-darmstadt.de"; }
          { host = "*.mathematik.tu-darmstadt.de"; user = "brandy"; }
          { host = "shells"; hostname = "shells.darmstadt.ccc.de"; }
          { host = "vorstand"; hostname = "vorstand.darmstadt.ccc.de"; }
          { host = "*.darmstadt.ccc.de"; user = "maralorn"; }
          { host = "whisky"; hostname = "whisky.w17.io"; user = "chaos"; }
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
    };
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

    pythonPackages.qrcode
    ranger


    (pkgs.neovim.override {
      vimAlias = true;
      configure = {
        customRC = builtins.readFile ./configs/vimrc;
        packages.myVimPackage = with pkgs.vimPlugins; {
          start = [
            deoplete-nvim
            vim-nix
            vimtex
            Syntastic
            UltiSnips
            airline
            rust-vim
            deoplete-rust
            fugitive
            airline
            vim-snippets
            vim-trailing-whitespace
            vim-racer
            vim-pandoc
            nerdcommenter
            vim-signify
          ];
        };
      };
      withPython3 = true;
    })
  ];
  xdg.enable = true;
}
