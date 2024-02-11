{ mkDerivation, base, containers, errors, exceptions, extra, feed
, filepath, filepattern, http-client, lens, lib, megaparsec
, notmuch, optparse-applicative, purebred-email, relude, say
, string-interpolate, tagsoup, text, time, witch, wreq
}:
mkDerivation {
  pname = "rssfeeds";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base containers errors exceptions extra feed filepath filepattern
    http-client lens megaparsec notmuch optparse-applicative
    purebred-email relude say string-interpolate tagsoup text time
    witch wreq
  ];
  license = "unknown";
}
