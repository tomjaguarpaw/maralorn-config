{
  makeHaskellScriptPackages = p: {
    inherit (p)
      aeson
      shh
      string-interpolate
      relude
      replace-megaparsec
      async
      say
      megaparsec
      fdo-notify
      these
      fsnotify
      witch
      ;
  };
}
