{ lib, config, pkgs, ... }:
let
  gpg = "6C3D12CD88CDF46C5EAF4D12226A2D41EF5378C9";
  name = "Malte Brandy";
  mail = "malte.brandy@maralorn.de";
  alternates = pkgs.privateValue [ ] "mail/alternates";
  lists = pkgs.privateValue { sortLists = [ ]; stupidLists = [ ]; notifications = [ ]; } "mail/filters";
  quick-sync = pkgs.writeShellScript "quick-mail-sync" ''
    ${pkgs.isync}/bin/mbsync hera:INBOX,Code,Move/todo
    ${pkgs.fd}/bin/fd -tf . ${maildir}/hera/Move/todo | ${pkgs.mblaze}/bin/mscan -f "E-Mail from %f: %S" | xargs -I '{}' ${pkgs.taskwarrior}/bin/task add '"{}"'
    ${pkgs.mblaze}/bin/mlist ${maildir}/hera/Move/todo | ${pkgs.mblaze}/bin/mrefile ${unsorted}
    ${pkgs.mblaze}/bin/mlist ${maildir}/hera/Move/todo | ${pkgs.mblaze}/bin/mflag -S
    ${pkgs.notmuch}/bin/notmuch new
  '';
  maildir = config.accounts.email.maildirBasePath;
  # mhdr -h List-ID -d Maildir/hera/Archiv/unsortiert | sort | sed 's/^.*<\(.*\)>$/\1/' | uniq | xargs -I '{}' sh -c "notmuch count List:{} | sed 's/$/: {}/'" | sort
  # To find candidates
  archiveSuffix = "hera/Archiv";
  unsortedSuffix = "${archiveSuffix}/unsortiert";
  unsorted = "${maildir}/${unsortedSuffix}";
  archive = "${maildir}/${archiveSuffix}";
  filter = rec {
    mailToFolder = name:
      toFolder (lib.concatStringsSep "." (lib.splitString "@" name));
    toFolder = name:
      lib.concatStringsSep "/" (lib.reverseList (lib.splitString "." name));
    simple = filter: target: { inherit filter target; };
    notifications = notify:
      simple "from:${notify}" "notifications/${mailToFolder notify}";
    stupidList = list: simple "to:${list}" "list/${mailToFolder list}";
    simpleSortList = listName:
      simple "List:${listName}" "list/${toFolder listName}";
  };
  myFilters = builtins.map filter.simpleSortList lists.sortLists
    ++ builtins.map filter.stupidList lists.stupidLists
    ++ builtins.map filter.notifications lists.notifications;
  sortMail = pkgs.writeHaskellScript
    {
      name = "sort-mail-archive";
      bins = [ pkgs.notmuch pkgs.coreutils pkgs.mblaze pkgs.findutils ];
      imports = [
        "Text.Megaparsec"
        "Text.Megaparsec.Char"
        "Text.Megaparsec.Char.Lexer"
        "qualified Data.List.NonEmpty as NE"
        "qualified Data.Text as T"
        "System.Environment (setEnv)"
      ];
    } ''
      reScan = notmuch "new" "--quiet"

      findFilterMail :: (Text,Text) -> IO (Maybe (LByteString, Text, Text))
      findFilterMail (filter_, target) = do
         files <- notmuch "search" "--output" "files" (toString filter_) "folder:${unsortedSuffix}" |> capture
         pure $ if (LBS.length files > 0) then Just (files, filter_, target) else Nothing

      executeFilterMail :: (LByteString, Text, Text) -> IO ()
      executeFilterMail (files, filter_, target) = do
         say [i|Sorting "#{filter_}" into #{target}|]
         writeOutput files |> mscan
         mmkdir ([i|${archive}/#{target}|] :: String)
         writeOutput files |> mrefile ([i|${archive}/#{target}|] :: String)

      myFilters :: [(Text,Text)]
      myFilters = [${
    lib.concatStringsSep ","
      (
        builtins.map ({ filter, target }: ''("${filter}","${target}")'')
          myFilters
      )
    }]

      filtersFromTo :: Text -> Maybe (Text,Text)
      filtersFromTo = filtersFromField "to" [toToName]
      toToName :: Text -> Maybe Text
      toToName (T.splitOn "@" -> [name, "maralorn.de"])
            | not (T.isInfixOf "randy" name) = Just . ("to/" <>) . T.intercalate "_" . T.splitOn "." $ name
      toToName _ = Nothing
      filtersFromField :: Text -> [Text-> Maybe Text] -> Text -> Maybe (Text,Text)
      filtersFromField field filters text = fmap ([i|#{field}:#{text}|],) . viaNonEmpty Relude.head . mapMaybe ($ text) $ filters
      filtersFromListIDs :: Text -> Maybe (Text,Text)
      filtersFromListIDs = filtersFromField "List" [githubNameFolderFromId, gitlabNameFolderFromId]
      githubNameFolderFromId :: Text -> Maybe Text
      githubNameFolderFromId (reverse . T.splitOn "." -> ("com":"github":org:name)) = Just [i|github/#{org}/#{T.intercalate "_" $ reverse name}|]
      githubNameFolderFromId _ = Nothing
      gitlabNameFolderFromId :: Text -> Maybe Text
      gitlabNameFolderFromId (reverse . T.splitOn "." -> ("de":"ccc":"darmstadt":"git":org:name1:name)) = Just [i|cda-gitlab/#{org}/#{T.intercalate "_" . toList . Relude.tail $ NE.reverse (name1:|name)}|]
      gitlabNameFolderFromId _ = Nothing

      type Parser = Parsec Text Text
      listId :: Parser Text
      listId = manyTill anySingle (char '<') *> (toText <$> manyTill anySingle (char '>'))

      main = do
         setEnv "MBLAZE_PAGER" "cat"
         setEnv "NOTMUCH_CONFIG" "${config.home.sessionVariables.NOTMUCH_CONFIG or ""}"
         reScan
         (listIDs,tos) <- concurrently (mhdr "-h" "List-ID" "-d" "${unsorted}" |> capture) (mhdr "-h" "To" "-d" "${unsorted}" "-A" |> capture)
         let listFilters = mapMaybe filtersFromListIDs . sortNub . mapMaybe (parseMaybe listId) . lines . decodeUtf8 $ listIDs
             toFilters = mapMaybe filtersFromTo . sortNub . fmap (\x -> maybe x Relude.id $ parseMaybe listId x) . lines . decodeUtf8 $ tos
         applicableFilters <- catMaybes <$> forConcurrently (listFilters <> myFilters <> toFilters) findFilterMail
         for_ applicableFilters executeFilterMail
         reScan
  '';
in
{

  services.mbsync = {
    enable = true;
    frequency = "*:0/30";
    verbose = false;
    postExec = "${sortMail}/bin/sort-mail-archive";
  };
  systemd.user.timers.mbsync.Timer.RandomizedDelaySec = "10m";

  accounts.email.accounts = pkgs.privateValue { } "mail/accounts";
  systemd.user.services =
    let
      mkService = name: account:
        let
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
            exports.onNotify = "${quick-sync}"
            exports.boxes = [ "Inbox", "Code" ];
          '';
        in
        {
          Unit.Description = "Run imapnotify for imap account ${name}";
          Service = {
            ExecStart = "${pkgs.imapnotify}/bin/imapnotify -c ${configjs}";
            Restart = "always";
            RestartSec = "1min";
          };
          Install.WantedBy = [ "default.target" ];
        };
      mkServiceWithName = name: account: {
        name = "imapnotify-${name}-inbox";
        value = mkService name account;
      };
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
    lib.mapAttrs' mkWatchService
      (lib.filterAttrs hasImapHost config.accounts.email.accounts) // lib.mapAttrs' mkServiceWithName
      (lib.filterAttrs hasImapHost config.accounts.email.accounts) // {
      mbsync.Service = {
        Environment = "PATH=${pkgs.coreutils}/bin";
        Restart = "on-failure";
        RestartSec = "30s";
      };
    };

  programs.msmtp.enable = true;
  programs.mbsync.enable = true;
  programs.notmuch = {
    enable = config.accounts.email.accounts != { };
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

  home = {
    packages = [ sortMail ];
    file =
      let
        mutt_alternates = "@maralorn.de "
          + (builtins.concatStringsSep " " alternates);
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
