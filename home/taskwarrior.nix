{ lib, pkgs, config, ... }: {
  services.taskwarrior-sync = {
    enable = true;
    frequency = "*:0/1";
  };
  home.file = let
    functions = ''
      printMap :: HashMap Text (These Value Value) -> String
      printMap = intercalate "\n" . fmap printPair . filter filterPairs . HM.toList
      filterPairs = \(k,_) -> k `notElem` ["uuid","entry","modified"]
      printPair = \(k,v) -> case v of
        This old -> [i|#{k}: -#{printValue old}|]
        That new -> [i|#{k}: +#{printValue new}|]
        These old new -> printDiff k old new
      printDiff :: Text -> Value -> Value -> String
      printDiff k (Array before) (Array after) = let
        old = (V.toList before) List.\\ (V.toList after)
        new = (V.toList after) List.\\ (V.toList before)
       in
        [i|#{k}: #{intercalate ", " ((('-':) . printValue <$> old) ++ (('+':) . printValue <$> new))}|]
      printDiff k old new = [i|#{k}: +#{printValue new} -#{printValue old}|]
      printValue = \case
        String (toString -> a) -> if | Just (d :: UTCTime) <- parseTimeM False defaultTimeLocale "%Y%m%dT%H%M%SZ" a -> show d
                                     | otherwise -> a
        Number a -> show a
        Array (fmap printValue -> a) -> intercalate "\n" (toList a)
        Object a -> show $ toList a
        Bool a -> show a
        Null -> "null"
    '';
    inherit (import ../lib) writeHaskellScript;
    libraries = [
      pkgs.haskellPackages.fdo-notify
      pkgs.haskellPackages.aeson
      pkgs.haskellPackages.these
    ];
    imports = [
      "DBus.Notify"
      "DBus.Client (clientErrorMessage)"
      "System.IO"
      "Data.Aeson"
      "Control.Monad"
      "Data.These"
      "qualified Data.HashMap.Strict as HM"
      "Data.Time"
      "qualified Data.Vector as V"
      "qualified Data.List as List"
    ];
    on-modify = writeHaskellScript {
      inherit imports libraries;
      name = "on-modify";
    } ''
      ${functions}
      main = do
       input1 <- BS.hGetLine stdin
       input2 <- BS.hGetLine stdin
       let description = do
            Object task1 <- decode $ toLazy input1 :: Maybe Value
            Object task2 <- decode $ toLazy input2 :: Maybe Value
            let diff = HM.unions [
                  fmap This (HM.difference task1 task2),
                  fmap That (HM.difference task2 task1),
                  (HM.mapMaybe (\x->x) $ HM.intersectionWith (\old new -> if old /= new then Just $ These old new else Nothing) task1 task2)
                 ]
            String description2 <- HM.lookup "description" task2
            pure (description2, printMap diff)
       Control.Monad.forM_ description $ \(d,b) -> do
         client <- try connectSession
         either
           (BS.hPut stderr . encodeUtf8 . (++ "\n") . clientErrorMessage)
           (\c -> void $ notify c blankNote { summary = [i|Modified task #{d}|], body = Just $ Text b, expiry = Milliseconds 15000 })
           client
       BS.hPut stdout input2
    '';
    on-add = writeHaskellScript {
      inherit imports libraries;
      name = "on-add";
    } ''
      ${functions}
      main = do
       input <- BS.hGetContents stdin
       let description = do
            Object task <- decode $ toLazy input :: Maybe Value
            pure . printMap . fmap That $ task
       whenJust description $ \d -> do
         client <- try connectSession
         either
           (BS.hPut stderr . encodeUtf8 . clientErrorMessage)
           (\c -> void $ notify c blankNote { summary = "New Task", body = Just $ Text d, expiry = Milliseconds 15000 })
           client
       BS.hPut stdout input
    '';
  in {
    "add-notification" = {
      target = ".task/hooks/on-add.notification";
      source = "${on-add}/bin/on-add";
    };
    "modify-notification" = {
      target = ".task/hooks/on-modify.notification";
      source = "${on-modify}/bin/on-modify";
    };
    "add-kassandra-notification" = {
      target = ".task/hooks/on-add.kassandra-notification";
      executable = true;
      text = ''
        #!${pkgs.bash}/bin/bash
        ${pkgs.coreutils}/bin/tee >(${pkgs.netcat}/bin/nc 127.0.0.1 6545)
      '';
    };
    "modify-kassandra-notification" = {
      target = ".task/hooks/on-modify.kassandra-notification";
      executable = true;
      text = ''
        #!${pkgs.bash}/bin/bash
        ${pkgs.coreutils}/bin/tail -n 1 | ${pkgs.coreutils}/bin/tee >(${pkgs.netcat}/bin/nc 127.0.0.1 6545)
      '';
    };
  };
  programs.taskwarrior = let cfg = config.m-0.private.taskwarrior;
  in {
    enable = true;
    dataLocation = "${config.home.homeDirectory}/.task";
    config = {
      taskd = {
        certificate = builtins.toFile "public.cert" cfg.publicCert;
        credentials = cfg.credentials;
        ca = builtins.toFile "ca.cert" cfg.caCert;
        key = builtins.toFile "private.key" cfg.privateKey;
        server = "hera.m-0.eu:53589";
      };
    };
    extraConfig = ''
      alias.inbox=+PENDING -TAGGED limit:1
      alias.inboxall=+PENDING -TAGGED

      verbose=blank,header,footnote,label,new-id,affected,edit,special,sync
      nag=


      uda.partof.type=string
      uda.partof.label=Parent task
      uda.generated.type=string
      uda.gen_name.type=string
      uda.gen_name.label=Generator name
      uda.gen_id.type=string
      uda.gen_id.label=Generator id
      uda.gen_orphan.type=string
      uda.gen_orphan.label=Generated orphan behavior
      uda.listposition.type=numeric
    '';
  };
}
