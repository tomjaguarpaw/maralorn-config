{
  pkgs,
  lib,
  config,
  ...
}:
let
  mail2task = pkgs.writeShellScript "mail2task" ''
    set -euo pipefail
    ${pkgs.isync}/bin/mbsync hera:Move/todo
    ${pkgs.fd}/bin/fd -tf . ${maildir}/hera/Move/todo | ${pkgs.mblaze}/bin/mscan -f "E-Mail from %f: %S" | ${pkgs.findutils}/bin/xargs '-d\n' -I '{}' ${pkgs.taskwarrior}/bin/task add '"{}"'
    ${pkgs.mblaze}/bin/mlist ${maildir}/hera/Move/todo | ${pkgs.mblaze}/bin/mflag -S
    ${pkgs.mblaze}/bin/mlist ${maildir}/hera/Move/todo | ${pkgs.mblaze}/bin/mrefile ${unsorted}
    ${pkgs.notmuch}/bin/notmuch new
  '';
  lists =
    pkgs.privateValue
      {
        sortLists = [];
        stupidLists = [];
        notifications = [];
      }
      "mail/filters";
  maildir = config.accounts.email.maildirBasePath;
  # mhdr -h List-ID -d Maildir/hera/Archiv/unsortiert | sort | sed 's/^.*<\(.*\)>$/\1/' | uniq | xargs -I '{}' sh -c "notmuch count List:{} | sed 's/$/: {}/'" | sort
  # To find candidates
  archiveSuffix = "hera/Archiv";
  unsortedSuffix = "${archiveSuffix}/unsortiert";
  unsorted = "${maildir}/${unsortedSuffix}";
  archive = "${maildir}/${archiveSuffix}";
  filter = rec {
    mailToFolder = name: toFolder (lib.concatStringsSep "." (lib.splitString "@" name));
    toFolder = name: lib.concatStringsSep "/" (lib.reverseList (lib.splitString "." name));
    simple = filter: target: {inherit filter target;};
    notifications = notify: simple "from:${notify}" "notifications/${mailToFolder notify}";
    stupidList = list: simple "to:${list}" "list/${mailToFolder list}";
    simpleSortList = listName: simple "List:${listName}" "list/${toFolder listName}";
  };
  myFilters =
    builtins.map filter.simpleSortList lists.sortLists
    ++ builtins.map filter.stupidList lists.stupidLists
    ++ builtins.map filter.notifications lists.notifications;
  sortMail =
    pkgs.writeHaskellScript
      {
        name = "sort-mail-archive";
        bins = [
          pkgs.notmuch
          pkgs.coreutils
          pkgs.mblaze
          pkgs.findutils
        ];
        imports = [
          "Text.Megaparsec"
          "Text.Megaparsec.Char"
          "Text.Megaparsec.Char.Lexer"
          "qualified Data.List.NonEmpty as NE"
          "qualified Data.Text as T"
          "System.Environment (setEnv)"
        ];
      }
      ''
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
          lib.concatStringsSep "," (builtins.map ({filter, target}: ''("${filter}","${target}")'') myFilters)
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
    postExec = "${sortMail}/bin/sort-mail-archive";
    preExec = toString mail2task;
  };
  accounts.email.accounts = {
    hera.imapnotify = {
      onNotifyPost = toString mail2task;
      boxes = ["Move/todo"];
    };
  };
}
