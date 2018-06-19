{ lib, config, pkgs, ...}:
with lib;
let
  mail = config.m-0.mail;
  me = config.m-0.private.me;
  mkMailbox = {password, user, host, name}:
  ''
    IMAPAccount ${name}
    Host ${host}
    User ${user}
    Pass ${password}
    SSLType IMAPS

    IMAPStore ${name}-remote
    Account ${name}

    MaildirStore ${name}-local
    Path ~/mail/${name}/
    Inbox ~/mail/${name}/INBOX/
    Subfolders Verbatim
    AltMap yes

    Channel ${name}
    Master :${name}-remote:
    Slave :${name}-local:
    Patterns *
    Create Both
    Expunge Both
    Remove Both
    SyncState *
  '';

in {
  options.m-0.mail.enable = mkEnableOption "private-mail";
  options.m-0.mail.boxes = mkOption {
    type = types.listOf types.attrs;
  };

  config = mkIf mail.enable {
    services.mbsync = {
      enable = true;
      configFile = builtins.toFile "mbsync-conf" (builtins.concatStringsSep "\n" (builtins.map mkMailbox mail.boxes));
      frequency = "*:0/1";
      verbose = false;
    };
    home = {
      packages = [ pkgs.neomutt ];
      file.".neomuttrc".text = ''
        set folder="~/mail/"
        mailboxes `find ~/mail -type d -name INBOX -printf '"%h" '` `find ~/mail -type d -name cur -printf '"%h" '`
        set sort=threads
        set realname="${me.name}"
        set spoolfile="~/mail/m-0/INBOX"
        set sidebar_folder_indent=yes
        set sidebar_short_path=yes
        set sidebar_visible=yes
        set sidebar_width=40
        set sidebar_sort_method="alpha"
        set sidebar_indent_string=" "
        set mail_check_stats=yes
        bind index,pager \CP sidebar-prev
        bind index,pager \CN sidebar-next
        bind index,pager \CO sidebar-open
        color sidebar_indicator black white
        color sidebar_highlight white blue
        set sidebar_format = "%B%* %?N?%N/?%S"
        set header_cache = "~/.cache/neomutt"
      '';
    };
  };
}
