{ lib, config, pkgs, ... }:
let
  gpg = "6C3D12CD88CDF46C5EAF4D12226A2D41EF5378C9";
  name = "Malte Brandy";
  mail = "malte.brandy@maralorn.de";
  alternates = pkgs.privateValue [ ] "mail/alternates";
  quick-sync = pkgs.writeShellScriptBin "quick-mail-sync" ''
    ${pkgs.isync}/bin/mbsync hera:INBOX,Code
    ${pkgs.notmuch}/bin/notmuch new
  '';
  maildir = config.accounts.email.maildirBasePath;
in
{

  services.mbsync = {
    enable = true;
    frequency = "*:0/30";
    verbose = false;
  };
  systemd.user.timers.mbsync.Timer.RandomizedDelaySec = "10m";

  accounts.email.accounts = pkgs.privateValue { } "mail/accounts";
  systemd.user.services =
    let
      hasImapHost = name: account: account.imap != null;
      mkWatchService = name: account: {
        name = "watch-${name}-maildir";
        value = {
          Unit.Description = "Watch maildir for changes for account ${name}";
          Service = {
            ExecStart = toString (pkgs.writeShellScript "watch-${name}-maildir" ''
              while ${pkgs.coreutils}/bin/sleep 1s; do
                ${quick-sync}
                ${pkgs.inotify-tools}/bin/inotifywait -e move,create,delete -r ${maildir}/${name}/Inbox ${maildir}/${name}/Code
              done
            '');
          };
          Install.WantedBy = [ "default.target" ];
        };
      };
    in
    lib.mapAttrs' mkWatchService (lib.filterAttrs hasImapHost config.accounts.email.accounts) //
    {
      mbsync.Service = {
        Environment = "PATH=${pkgs.coreutils}/bin";
        Restart = "on-failure";
        RestartSec = "30s";
      };
    };

  programs = {
    msmtp.enable = true;
    mbsync.enable = true;
    notmuch = {
      enable = pkgs.withSecrets;
      hooks.postInsert = ''
        ${pkgs.notmuch}/bin/notmuch tag +deleted -- "folder:/Trash/ (not tag:deleted)"
        ${pkgs.notmuch}/bin/notmuch tag -deleted -- "(not folder:/Trash/) tag:deleted"
        ${pkgs.notmuch}/bin/notmuch tag +spam -- "folder:/Junk|Spam|SPAM/ (not tag:spam)"
        ${pkgs.notmuch}/bin/notmuch tag -spam -- "(not folder:/Junk|Spam|SPAM/) tag:spam"
      '';
      new = {
        tags = [ ];
        ignore = [ ".isyncuidmap.db" ];
      };
      maildir.synchronizeFlags = true;
    };
  };

  home = {
    packages = [ quick-sync ];
    file =
      let
        mutt_alternates = "@maralorn.de " + (builtins.concatStringsSep " " alternates);
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
          text/html; ${pkgs.lynx}/bin/lynx -assume_charset=%{charset} -display_charset=utf-8 -collapse_br_tags -dump %s; nametemplate=%s.html; copiousoutput
          application/*; ${pkgs.xdg_utils}/bin/xdg-open %s > /dev/null
          image/*; ${pkgs.xdg_utils}/bin/xdg-open %s > /dev/null
          video/*; ${pkgs.xdg_utils}/bin/xdg-open %s > /dev/null
          audio/*; ${pkgs.xdg_utils}/bin/xdg-open %s > /dev/null
        '';
      in
      {
        ".neomuttrc".text = ''
          set editor = "vim"
          alternative_order text/plain text/html
          auto_view text/*
          auto_view message/*
          unset wait_key
          color normal default default

          macro index,pager a ":set confirmappend=no\n<save-message>=hera/Archiv/unsortiert\n:set confirmappend=yes\n" "move message to archive"
          macro index,pager s ":set confirmappend=no\n<save-message>=hera/Junk\n:set confirmappend=yes\n" "move message to spam"
          macro index,pager t ":set confirmappend=no\n<save-message>=hera/Move/todo\n:set confirmappend=yes\n" "move message to todo list"
          macro index,pager l ":set confirmappend=no\n<save-message>=hera/Move/readlater\n:set confirmappend=yes\n" "move message to readlater list"
          macro attach 'V' "<pipe-entry>iconv -c --to-code=UTF8 > ~/.cache/mutt/mail.html<enter><shell-escape>firefox ~/.cache/mutt/mail.html<enter>"

          macro index,pager <F6> "<shell-escape>${pkgs.zsh}/bin/zsh -c '${pkgs.sieve-connect}/bin/sieve-connect -s ${config.accounts.email.accounts.hera.imap.host or ""} -u ${config.accounts.email.accounts.hera.userName or ""} --passwordfd 3 --edit --remotesieve filter 3<<(pass eu/m-0/hera/mail.hera.m-0.eu/maralorn)'\n"
          macro index,pager A "<pipe-message>${pkgs.khard}/bin/khard add-email<return>" "add sender to to khard"

          set query_format="%4c %t %-70.70a %-70.70n %?e?(%e)?"
          set query_command = "${pkgs.notmuch}/bin/notmuch address --output=recipients --deduplicate=address '%s' | grep -i '%s'"
          bind editor <Tab> complete-query
          bind editor ^T complete

          set delete = yes
          set crypt_use_gpgme = yes
          set crypt_replysignencrypted = yes
          set crypt_verify_sig = yes
          set pgp_use_gpg_agent = yes
          set pgp_auto_decode = yes
          set pgp_autosign = no
          set pgp_replysign = yes
          set pgp_replyencrypt = yes
          set pgp_sign_as="${gpg}"
          set pgp_default_key="${gpg}"
          set timeout = 5
          set ts_enabled = yes

          set abort_noattach = ask-no
          set abort_noattach_regex = "(hängt an|anhäng|anhang|anbei|angehängt|attach|attached|attachments?)"
          set abort_unmodified = ask-yes

          alternates ${mutt_alternates}
          set folder="${maildir}"
          mailboxes `find -L ${maildir} -type d -name Inbox -printf '"%h" '` `find -L ${maildir} -type d -name cur -printf '"%h" '`
          set sendmail="${pkgs.msmtp}/bin/msmtp --read-envelope-from"
          set sort=threads
          set sort_aux=last-date-received
          set realname="${name}"
          set from=fill-later
          set use_from=yes
          set fast_reply=yes
          set mailcap_path=${mailcap};
          set include=yes
          set edit_headers=yes
          set mbox_type=Maildir
          set spoolfile="${maildir}/hera/Archiv"
          set record="${maildir}/hera/Archiv/unsortiert"
          set postponed="${maildir}/hera/Drafts"
          set mail_check_stats=yes
          bind index / vfolder-from-query
          set header_cache = "~/.cache/neomutt"
          set date_format="!%y-%m-%d %H:%M"
          set mime_forward=yes
          set mime_forward_rest=yes

          macro index <F5> "!${quick-sync} > /dev/null<enter>"

          source "${hide-sidebar}"
          macro index <right> "<enter-command>source ${hide-sidebar}<enter>"
          macro index <left> "<enter-command>source ${show-sidebar}<enter>"
          set sidebar_folder_indent=no
          set sidebar_short_path=no
          set sidebar_component_depth=2
          set sidebar_width=60
          set sidebar_sort_method="alpha"
          set sidebar_indent_string=" "
          color sidebar_indicator black white
          color sidebar_highlight white blue
          set sidebar_format = "%B%* %?N?%N/?%S"

          set date_format="%F %R"
          set index_format="%Z %D %-15.15L %s"

          ignore *
          unignore from date cc bcc to subject list-unsubscribe

          alias f__0 ${name} <${mail}>
          ${builtins.concatStringsSep "\n"
          (
            lib.imap1 (n: x: "alias f__${toString n} ${name} <${x}>")
              alternates
          )}
          send2-hook '~f fill-later' "push <edit-from><kill-line>f__<complete><search>${mail}<enter>"
          set query_command = "${pkgs.khard}/bin/khard email --parsable %s"
        '';
      };
  };

}
