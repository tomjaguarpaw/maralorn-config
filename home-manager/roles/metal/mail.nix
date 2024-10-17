{
  lib,
  config,
  pkgs,
  ...
}:
let
  inherit
    (pkgs.privateValue {
      gpg = "";
      name = "";
      mail = "";
      alternates = [ ];
    } "mail/me")
    gpg
    name
    alternates
    ;
  sync = "PATH=${lib.makeBinPath [ pkgs.coreutils ]}:$PATH ${lib.getExe' pkgs.isync "mbsync"}";
  quick-mail-sync = pkgs.writeShellScriptBin "quick-mail-sync" ''
    ${pkgs.coreutils}/bin/mkdir -p ~/.cache/mutt
    ${sync} hera:INBOX hera:Move/todo hera:Move/readlater hera:Junk heilmann-inbox
    ${
      if config.m-0.hostName == "athene" then lib.getExe' pkgs.vikunja-tools "vikunja-mail-import" else ""
    }
    ${lib.getExe pkgs.notmuch} new
  '';
  maildir = config.accounts.email.maildirBasePath;
in
{
  services = {
    mbsync = {
      enable = true;
      frequency = "*:0/30";
      verbose = false;
    };
    imapnotify.enable = true;
  };
  systemd.user.timers.mbsync.Timer.RandomizedDelaySec = "10m";

  accounts.email.accounts = lib.recursiveUpdate (pkgs.privateValue { } "mail/accounts") {
    hera = {
      passwordCommand = "${pkgs.coreutils}/bin/cat /run/agenix/mail-password";
      imapnotify.onNotify = lib.getExe quick-mail-sync;
      imapnotify.extraConfig.onDeletedMail = lib.getExe quick-mail-sync;
    };
    heilmann = {
      passwordCommand = "${pkgs.coreutils}/bin/cat /run/agenix/heilmann-mail-password";
      imapnotify.onNotify = lib.getExe quick-mail-sync;
      imapnotify.extraConfig.onDeletedMail = lib.getExe quick-mail-sync;
    };
  };

  systemd.user.services =
    let
      hasImapHost = _name: account: account.imap != null;
      mkWatchService = name: _account: {
        name = "watch-${name}-maildir";
        value = {
          Unit.Description = "Watch maildir for changes for account ${name}";
          Service = {
            ExecStart = toString (
              pkgs.writeShellScript "watch-${name}-maildir" ''
                if [[ -d ${maildir}/${name}/Inbox ]]; then
                  while ${pkgs.coreutils}/bin/sleep 1s; do
                    ${lib.getExe quick-mail-sync}
                    ${pkgs.inotify-tools}/bin/inotifywait -e move,create,delete -r ${maildir}/${name}/Inbox ${
                      if name == "hera" then "${maildir}/${name}/Code" else ""
                    } --exclude "\.lock"
                  done
                fi
              ''
            );
          };
          Install.WantedBy = [ "default.target" ];
        };
      };
    in
    lib.mapAttrs' mkWatchService (lib.filterAttrs hasImapHost config.accounts.email.accounts)
    // {
      mbsync = {
        Service = {
          Environment = "PATH=${lib.makeBinPath [ pkgs.coreutils ]}";
          Restart = "on-failure";
          RestartSec = "30s";
        };
        Unit.StartLimitIntervalSec = "3m";
      };
    };

  programs = {
    msmtp.enable = true;
    mbsync.enable = true;
    notmuch = {
      enable = true;
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
    packages = [ quick-mail-sync ];
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
        # See: https://unix.stackexchange.com/questions/44358/mutt-mark-as-read-and-delete
        move-message-macro =
          account: key: dir:
          ''folder-hook ${account} "macro index,pager ${key} \":set confirmappend=no resolve=no\n<clear-flag>N<save-message>=${account}/${dir}\n:set confirmappend=yes resolve=yes\n<next-undeleted>\" \"move message to ${account}/${dir}\""'';
      in
      {
        ".neomuttrc".text = ''
          set editor = "hx"
          alternative_order text/plain text/html
          auto_view text/*
          auto_view message/*
          set wait_key = false
          color normal default default

          ${move-message-macro "hera" "a" "Archiv/unsortiert"}
          ${move-message-macro "heilmann" "a" "Archive"}
          ${move-message-macro "hera" "s" "Junk"}
          ${move-message-macro "heilmann" "s" "Spam"}
          ${move-message-macro "hera" "t" "Move/todo"}
          ${move-message-macro "hera" "l" "Move/readlater"}
          macro attach 'V' "<shell-escape>mkdir -p ~/.cache/mutt<enter><pipe-entry>iconv -c --to-code=UTF8 > ~/.cache/mutt/mail.html<enter><shell-escape>firefox ~/.cache/mutt/mail.html<enter>"

          macro index,pager <F6> "<shell-escape>${pkgs.zsh}/bin/zsh -c '${pkgs.sieve-connect}/bin/sieve-connect -s ${
            config.accounts.email.accounts.hera.imap.host or ""
          } -u ${
            config.accounts.email.accounts.hera.userName or ""
          } --passwordfd 3 --edit --remotesieve filter 3<<(cat /run/agenix/mail-password)'\n"
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
          mailboxes `find -L ${maildir} -type d -name cur -printf '"%h" '`
          set sendmail="${pkgs.msmtp}/bin/msmtp --read-envelope-from"
          set sort=threads
          set sort_aux=last-date-received
          set realname="${name}"
          set from=malte.ott@maralorn.de
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

          macro index <F5> "!${lib.getExe quick-mail-sync} > /dev/null<enter>"

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

          set query_command = "${pkgs.khard}/bin/khard email --parsable %s"
        '';
        #alias f__0 ${name} <${mail}>
        #${
        #  builtins.concatStringsSep "\n"
        #  (
        #    lib.imap1 (n: x: "alias f__${toString n} ${name} <${x}>")
        #    alternates
        #  )
        #}
        #send2-hook '~f fill-later' "push <edit-from><kill-line>f__<complete><search>${mail}<enter>"
      };
  };
}
