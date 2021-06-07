{ pkgs, config, lib, ... }: {

  imports = [
    ./zsh
    ./home-options.nix
    ../../common
    ./unlock.nix
    ./mpclient.nix
    ./neovim
    ./nvd.nix
  ];
  nixpkgs.overlays = import ../../overlays { inherit lib; };

  programs = {
    home-manager.enable = true;
    nix-index = {
      enable = true;
      enableZshIntegration = true;
    };
    exa = {
      enable = true;
      enableAliases = true;
    };
    lazygit = {
      settings = {
        customCommands = [
          {
            key = "s";
            command = "git sync";
            context = "global";
            subprocess = true;
          }
          {
            key = "s";
            command = "git sync";
            context = "files";
            subprocess = true;
          }
        ];
      };
      enable = true;
    };
    direnv = {
      enable = true;
      enableZshIntegration = true;
      enableNixDirenvIntegration = true;
    };
    tmux = {
      enable = true;
      escapeTime = 1;
      historyLimit = 50000;
      terminal = "screen-256color";
      extraConfig = ''
        set -g set-titles on
        set -g status off
        set -g set-titles-string '#T - #W - tmux @ #h'
        set -g allow-rename on
      '';
    };
    password-store = {
      package = pkgs.pass-wayland.withExtensions (exts: [ exts.pass-update pkgs.pass-clip exts.pass-otp ]);
      enable = true;
      settings.PASSWORD_STORE_DIR = "${config.home.homeDirectory}/git/password-store";
    };
    git = {
      aliases = {
        sync = "!git pull -r && git push";
        cpr = "!f() { git fetch origin refs/pull/$1/head && git checkout FETCH_HEAD; }; f";
      };
      extraConfig = {
        pull.ff = "only";
        core.editor = "nvim";
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
      userEmail = "malte.brandy@maralorn.de";
      userName = "Malte Brandy";
    };
    htop = {
      enable = true;
      settings = {
        hide_threads = true;
        hide_userland_threads = true;
        highlight_base_name = true;
        shadow_other_users = true;
        show_program_path = false;
        tree_view = true;
        sort_key = "USER";
      };
    };
    ssh = {
      controlMaster = "auto";
      controlPersist = "120";
      enable = true;
      matchBlocks =
        let
          agHost = "fb04217.mathematik.tu-darmstadt.de";
        in
        {
          athene.hostname = "192.168.178.22";
          git-auto = {
            hostname = "hera.m-0.eu";
            user = "git";
            identityFile = "~/.ssh/id_auto_ed25519";
          };
          git = {
            hostname = "hera.m-0.eu";
            user = "git";
          };
          hera = {
            hostname = "hera.m-0.eu";
            user = "maralorn";
          };
          ag-forward = {
            hostname = agHost;
            proxyJump = "gw";
            user = "brandy";
          };
          ag = {
            hostname = agHost;
            user = "brandy";
          };
          gw = {
            hostname = "gwres4.mathematik.tu-darmstadt.de";
            user = "brandy";
          };
          shells = {
            hostname = "shells.darmstadt.ccc.de";
            user = "maralorn";
          };
          whisky = {
            hostname = "whisky.w17.io";
            user = "chaos";
          };
          kitchen = {
            hostname = "kitchen.w17.io";
            user = "chaos";
          };
          "door.w17.io".identityFile = "~/.ssh/door_rsa";
        };
    };
  };

  home = {
    packages = builtins.attrValues pkgs.home-pkgs ++ [
      (
        pkgs.writeShellScriptBin "unlock-ssh" ''
          SSH_ASKPASS="print-ssh-pw" DISPLAY="a" ssh-add < /dev/null
        ''
      )
      (
        pkgs.writeShellScriptBin "print-radicle-pw"
          "pass show etc/radicle/${config.m-0.hostName}"
      )
      (
        pkgs.writeShellScriptBin "print-ssh-pw"
          "pass show eu/m-0/${config.m-0.hostName}.m-0.eu/ssh-key"
      )
    ];
    sessionVariables = {
      PATH = "$HOME/.nix-profile/bin:$PATH";
      BROWSER = "${pkgs.firefox}/bin/firefox";
      EMAIL = "malte.brandy@maralorn.de";
      SUDO_ASKPASS = toString (
        pkgs.writeShellScript "print-sudo-pw"
          "pass show eu/m-0/${config.m-0.hostName}.m-0.eu/${config.home.username}"
      );
    };
  };

  systemd.user.startServices = true;

  services = {
    gpg-agent = {
      enable = true;
      defaultCacheTtl = 31536000; # 1year
      maxCacheTtl = 31536000; # 1year
    };
  };

  xdg.enable = true;
}
