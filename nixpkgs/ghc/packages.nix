{
  makeHaskellScriptPackages = p: {
    inherit (p)
      aeson
      tidal
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
      postgresql-simple
      HTTP
      ;
  };
}
