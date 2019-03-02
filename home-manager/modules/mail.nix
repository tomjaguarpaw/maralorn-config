{ lib, config, pkgs, ...}:
with lib;
let
  mail = config.m-0.mail;
  inherit (config.m-0.private) sendmail me;
  maildir = config.accounts.email.maildirBasePath;
in {

options.m-0.mail.enable = mkEnableOption "private-mail";
options.m-0.mail.accounts = mkOption {
  type = types.attrs;
};

config = mkIf mail.enable {

services.mbsync = {
  enable = true;
  frequency = "*:0/30";
  verbose = false;
  postExec = "${pkgs.notmuch}/bin/notmuch --config=${config.home.sessionVariables.NOTMUCH_CONFIG} new";
};

accounts.email.accounts = config.m-0.mail.accounts;

systemd.user.services = let
  mkService = name: account: let
    configjs = pkgs.writeText "config.js" ''
      var child_process = require('child_process');

      function getStdout(cmd) {
          var stdout = child_process.execSync(cmd);
          return stdout.toString().trim();
      }

      exports.host = "${account.imap.host}"
      exports.port = 993
      exports.tls = true;
      exports.tlsOptions = { "rejectUnauthorized": false };
      exports.username = "${account.userName}";
      exports.password = getStdout("${toString account.passwordCommand}");
      exports.onNotify = "${pkgs.isync}/bin/mbsync ${name}"
      exports.onNotifyPost = "${pkgs.notmuch}/bin/notmuch new"
      exports.boxes = [ "Inbox" ];
    '';
  in
    {
      Unit = {
        Description = "Run imapnotify for imap account ${name}";
      };
      Service = {
        ExecStart= "${pkgs.imapnotify}/bin/imapnotify -c ${configjs}";
        Restart = "always";
        RestartSec = "1min";
      };
      Install = {
        WantedBy = [ "default.target" ];
      };
    };
  mkServiceWithName = name: account: {
    name = "imapnotify-${name}-inbox";
    value = mkService name account;
  };
  hasImapHost = name: account: account.imap != null;
in
  mapAttrs' mkServiceWithName (filterAttrs hasImapHost config.accounts.email.accounts);

programs.msmtp.enable = true;
programs.mbsync.enable = true;
programs.notmuch = {
  enable = true;
  hooks.postInsert = ''
    ${pkgs.notmuch}/bin/notmuch tag +deleted -- "folder:/Trash/ (not tag:deleted)"
    ${pkgs.notmuch}/bin/notmuch tag -deleted -- "(not folder:/Trash/) tag:deleted"
    ${pkgs.notmuch}/bin/notmuch tag +spam -- "folder:/Junk|Spam|SPAM/ (not tag:spam)"
    ${pkgs.notmuch}/bin/notmuch tag -spam -- "(not folder:/Junk|Spam|SPAM/) tag:spam"
  '';
  new = {
    tags = [];
    ignore = [ ".isyncuidmap.db" ];
  };
  maildir.synchronizeFlags = true;
};
  home = {
    packages = with pkgs; [
      neomutt
    ];
    file = let
        mutt_alternates = "@maralorn.de " + (builtins.concatStringsSep " " me.alternates);
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
        mailcap = pkgs.writeText "mailcap" ''
          text/html; ${pkgs.lynx}/bin/lynx -stdin -dump -force_html ; copiousoutput
          application/*; ${pkgs.xdg_utils}/bin/xdg-open %s > /dev/null
          image/*; ${pkgs.xdg_utils}/bin/xdg-open %s > /dev/null
          video/*; ${pkgs.xdg_utils}/bin/xdg-open %s > /dev/null
          audio/*; ${pkgs.xdg_utils}/bin/xdg-open %s > /dev/null
        '';
      in {
      ".neomuttrc".text = ''
        alternative_order text/plain text/html
        auto_view text/*
        auto_view message/*
        unset wait_key

        set query_format="%4c %t %-70.70a %-70.70n %?e?(%e)?"
        set query_command = "${pkgs.notmuch}/bin/notmuch address --output=recipients --deduplicate=address '%s' | grep -i '%s'"
        bind editor <Tab> complete-query
        bind editor ^T complete

        set crypt_use_gpgme = yes
        set pgp_use_gpg_agent = yes
        set pgp_auto_decode = yes
        set pgp_autosign = yes
        set pgp_replysign = yes
        set pgp_replyencrypt = yes
        set crypt_replysignencrypted = yes
        set crypt_verify_sig = yes
        set pgp_sign_as="${me.gpg}"
        set pgp_use_gpg_agent = yes
        set pgp_default_key="${me.gpg}"
        set timeout = 5


        alternates ${mutt_alternates}
        set folder="${maildir}"
        mailboxes `find ${maildir} -type d -name Inbox -printf '"%h" '` `find ${maildir} -type d -name cur -printf '"%h" '`
        set sendmail="${pkgs.msmtp}/bin/msmtp --read-envelope-from"
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
        set spoolfile="${maildir}/hera/Inbox"
        set record="${maildir}/hera/Archiv/unsortiert"
        set postponed="${maildir}/hera/Drafts"
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
      '';
    };
  };
};

}
