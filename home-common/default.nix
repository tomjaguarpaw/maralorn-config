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
      initExtra = ''
        if [[ -z "$TMUX" ]] {
          session=$(tmux ls | grep -v attached | head -1 | cut -f1 -d:)
          if [[ -n $session ]] {
            exec tmux attach -t $session;
          } else {
            exec tmux;
          }
        }
        precmd() {
          local s=$? c=( $(fc -l -D -1 ) )
          if [[ $launched && "''${c[2]}" != "0:00" ]] {
            eventc command $([[ ''${s} == 0 ]] && echo success || echo failure) -d command="\"''${c[3,-1]}\"" -d time="\"''${c[2]}\"" -d host="\"$HOST\""
          } else {
            export launched=true;
          }
        }

        alias c=cdr
        alias s='sudo systemctl'
        alias u='systemctl --user'
        alias m=man
        alias t="tmux attach"
        alias tn="tmux new-session"
        alias w="develop-here"
        alias ls=exa

        export BROWSER=qutebrowser
        export EDITOR=nvim
        export MANPAGER="most -s"
      '';
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
        customRC = ''
          set spell spelllang=de,en
          set background=dark
          set autoindent
          set nosmartindent
          set listchars=tab:»\ ,trail:.,extends:#
          set list
          set ts=3
          set number
          set scrolloff=5
          set sidescrolloff=5
          set laststatus=2
          set incsearch
          set mouse=
          set dir=~/.vimhist/
          set backupdir=~/.vimhist/bak
          set showcmd
          nnoremap <silent><cr> :nohlsearch<CR>
          vnoremap < <gv
          vnoremap > >gv
          nnoremap <silent><cr> :nohlsearch<CR>
          vnoremap < <gv
          vnoremap > >gv  u
          nnoremap <C-Down> <C-W><C-J>
          nnoremap <C-Up> <C-W><C-K>
          nnoremap <C-Right> <C-W><C-L>
          nnoremap <C-Left> <C-W><C-H>
          nnoremap <A-Left> gT
          nnoremap <A-Right> gt

          set colorcolumn=81,121
          hi ColorColumn ctermbg=black

          set winaltkeys=no
          set noai
          set si
          set sw=3
          set pt=<F4>
          set ignorecase
          set wildmenu
          set hlsearch
          noremap  <buffer> <silent> <Up>   gk
          noremap  <buffer> <silent> <Down> gj
          noremap  <buffer> <silent> <Home> g<Home>
          noremap  <buffer> <silent> <End>  g<End>
        '';
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
          ];
        };
      };
      withPython3 = true;
    })
  ];
  xdg.enable = true;
}
