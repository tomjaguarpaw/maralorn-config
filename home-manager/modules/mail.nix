{ lib, config, pkgs, ...}:
with lib;
let
  mail = config.m-0.mail;
  inherit (config.m-0.private) sendmail me;
  mkMailbox = {password, user, host, name}: ''
    IMAPAccount ${name}
    Host ${host}
    User ${user}
    PassCmd "${pkgs.pass}/bin/pass ${password}"
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

  mkSendconfig = {password, user, host, name, from ? null, default ? false , start_tls ? true , ... }: ''
    account ${name}
    host ${host}
    port ${if start_tls then "587" else "465"}
    tls_starttls ${if start_tls then "on" else "off"}
    user ${user}
    passwordeval "${pkgs.pass}/bin/pass ${password}"
    ${lib.optionalString (from != null) "from ${from}"}
    ${lib.optionalString default "account default: ${name}"}
  '';

in {

options.m-0.mail.enable = mkEnableOption "private-mail";
options.m-0.mail.boxes = mkOption {
  type = types.listOf types.attrs;
};
options.m-0.mail.sendmail = mkOption {
  type = types.listOf types.attrs;
};
options.m-0.mail.default = mkOption {
  type = types.attrs;
};

config = mkIf mail.enable {
  services.mbsync = {
    enable = true;
    configFile = pkgs.writeText "mbsync-conf" (builtins.concatStringsSep "\n" (builtins.map mkMailbox mail.boxes));
    frequency = "*:0/1";
    verbose = false;
    postExec = "${(pkgs.writeShellScriptBin "mail-filter" ''
      ${pkgs.notmuch}/bin/notmuch new
      ${pkgs.notmuch}/bin/notmuch tag -new -- tag:new
      ${pkgs.notmuch}/bin/notmuch tag +deleted -- "folder:/Trash/ (not tag:deleted)"
      ${pkgs.notmuch}/bin/notmuch tag -deleted -- "(not folder:/Trash/) tag:deleted"
      ${pkgs.notmuch}/bin/notmuch tag +spam -- "folder:/Spam|SPAM/ (not tag:spam)"
      ${pkgs.notmuch}/bin/notmuch tag -spam -- "(not folder:/Spam|SPAM/) tag:spam"
    '')}/bin/mail-filter";
  };
  home = {
    packages = with pkgs; [
      neomutt
      msmtp
      notmuch
    ];
    file = let
        msmtprc = pkgs.writeText "msmtprc" (''
          defaults
          auth           on
          tls            on
          tls_trust_file /etc/ssl/certs/ca-certificates.crt
        '' + (builtins.concatStringsSep "\n" (builtins.map mkSendconfig mail.sendmail)));
        mutt_alternates = "@maralorn.de " + (builtins.concatStringsSep " " me.alternates);
        notmuch_alternates = builtins.concatStringsSep ";" me.alternates;
        show-sidebar = pkgs.writeText "show-sidebar" ''
          set sidebar_visible=yes
          bind index <up> sidebar-prev
          bind index <down> sidebar-next
          bind index <pageup> sidebar-page-up
          bind index <pagedown> sidebar-page-down
          bind index <space> sidebar-open
          bind index <return> sidebar-open
          bind index <enter> sidebar-open
          '';
        hide-sidebar = pkgs.writeText "hide-sidebar" ''
          set sidebar_visible=no
          bind index <up> previous-undeleted
          bind index <down> next-undeleted
          bind index <pageup> previous-page
          bind index <pagedown> next-page
          bind index <space> display-message
          bind index <return> display-message
          bind index <enter> display-message
          '';
        mkFccString = { from , name,  sent }: ''
          send2-hook '${from}' "macro compose y <edit-fcc><kill-line>${config.home.homeDirectory}/mail/${name}/${sent}<enter><send-message>"
        '';
        mkFcc = { from ? null, name,  sent ? null, ... }:
          lib.optionalString
            (from != null && sent != null)
            (mkFccString { inherit name sent; from = "=f ${from}"; });
        mailcap = pkgs.writeText "mailcap" ''
          text/html; ${pkgs.lynx}/bin/lynx -stdin -dump -force_html ; copiousoutput
          application/*; ${pkgs.xdg_utils}/bin/xdg-open %s > /dev/null
          image/*; ${pkgs.xdg_utils}/bin/xdg-open %s > /dev/null
          video/*; ${pkgs.xdg_utils}/bin/xdg-open %s > /dev/null
          audio/*; ${pkgs.xdg_utils}/bin/xdg-open %s > /dev/null
        '';
      in {
      ".notmuch-config".text = ''
        [database]
        path=${config.home.homeDirectory}/mail

        [user]
        name=${me.name}
        primary_email=${me.mail}
        other_email=${notmuch_alternates}

        [new]
        tags=new;
        ignore=

        [search]
        exclude_tags=deleted;spam;

        [maildir]
        synchronize_flags=true
      '';
      ".neomuttrc".text = ''
        alternative_order text/plain text/html
        auto_view text/*
        auto_view message/*
        unset wait_key

        set query_format="%4c %t %-70.70a %-70.70n %?e?(%e)?"
        set query_command = "${pkgs.notmuch}/bin/notmuch address --output=recipients --deduplicate=address '%s' | grep -i '%s'"
        bind editor <Tab> complete-query
        bind editor ^T complete


        alternates ${mutt_alternates}
        set folder="${config.home.homeDirectory}/mail/"
        mailboxes `find ${config.home.homeDirectory}/mail -type d -name INBOX -printf '"%h" '` `find ~/mail -type d -name cur -printf '"%h" '`
        set sendmail="${pkgs.msmtp}/bin/msmtp -C${msmtprc} --read-envelope-from"
        set sort=threads
        set sort_aux=date-sent
        set realname="${me.name}"
        set from=fill-later
        set use_from=yes
        set fast_reply=yes
        set mailcap_path=${mailcap};
        set include=yes
        set edit_headers=yes
        set mbox_type=Maildir
        set spoolfile="${config.home.homeDirectory}/mail/${mail.default.name}/INBOX"
        set mail_check_stats=yes
        bind index / vfolder-from-query
        set header_cache = "~/.cache/neomutt"
        set date_format="!%y-%m-%d %H:%M"
        set mime_forward=yes
        set mime_forward_rest=yes

        macro index <F5> "!systemctl --user start mbsync > /dev/null<enter>"

        source "${hide-sidebar}"
        macro index <right> "<enter-command>source ${hide-sidebar}<enter>"
        macro index <left> "<enter-command>source ${show-sidebar}<enter>"
        set sidebar_folder_indent=yes
        set sidebar_short_path=yes
        set sidebar_width=40
        set sidebar_sort_method="alpha"
        set sidebar_indent_string=" "
        color sidebar_indicator black white
        color sidebar_highlight white blue
        set sidebar_format = "%B%* %?N?%N/?%S"

        alias f__0 ${me.name} <${me.mail}>
        ${builtins.concatStringsSep "\n" (lib.imap1 (n: x: "alias f__${toString n} ${me.name} <${x}>") me.alternates)}
        send2-hook '~f fill-later' "push <edit-from><kill-line>f__<complete><search>${me.mail}<enter>"
        ${mkFccString  { from = "~A"; inherit (mail.default) name sent; }}
        ${builtins.concatStringsSep "\n" (builtins.map mkFcc mail.sendmail)}
      '';
    };
  };
};

}
