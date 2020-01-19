{ lib, pkgs, config, ... }: {
  services.taskwarrior-sync = {
    enable = true;
    frequency = "*:0/1";
  };
  home.file = let
    functions = ''
      printMap :: HashMap Text (These Value Value) -> String
      printMap = intercalate "\n" . fmap printPair . ClassyPrelude.filter filterPairs . HM.toList
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
        String (unpack -> a) -> if | Just (d :: UTCTime) <- parseTimeM False defaultTimeLocale "%Y%m%dT%H%M%SZ" a -> show d
                                   | otherwise -> a
        Number a -> show a
        Array (fmap printValue -> a) -> intercalate "\n" a
        Object a -> show $ HM.toList a
        Bool a -> show a
        Null -> "null"
    '';
    inherit (import ../lib) writeHaskellScript unstable;
    libraries = [
      pkgs.haskellPackages.fdo-notify
      pkgs.haskellPackages.aeson
      pkgs.haskellPackages.these
    ];
    imports = [
      "DBus.Notify"
      "System.IO"
      "Data.Aeson"
      "Control.Monad"
      "Data.These"
      "Data.HashMap.Strict as HM"
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
            Object task1 <- decode $ LBSC.fromStrict input1 :: Maybe Value
            Object task2 <- decode $ LBSC.fromStrict input2 :: Maybe Value
            let diff = HM.unions [
                  fmap This (HM.difference task1 task2),
                  fmap That (HM.difference task2 task1),
                  (HM.mapMaybe (\x->x) $ HM.intersectionWith (\old new -> if old /= new then Just $ These old new else Nothing) task1 task2)
                 ]
            String description <- HM.lookup "description" task2
            pure (description, printMap diff)
       Control.Monad.forM_ description $ \(d,b) -> do
         client <- connectSession
         notify client blankNote { summary = [i|Modified task #{d}|], body = Just $ Text b, expiry = Milliseconds 15000 }
       hPut stdout input2
    '';
    on-add = writeHaskellScript {
      inherit imports libraries;
      name = "on-add";
    } ''
      ${functions}
      main = do
       input <- LBSC.hGetContents stdin
       let description = do
            Object task <- decode input :: Maybe Value
            pure . printMap . fmap That $ task
       Control.Monad.forM_ description $ \d -> do
         client <- connectSession
         notify client blankNote { summary = "New Task", body = Just $ Text d, expiry = Milliseconds 15000 }
       LBS.hPut stdout input
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
