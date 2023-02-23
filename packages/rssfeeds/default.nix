{
  mkDerivation,
  base,
  containers,
  errors,
  exceptions,
  extra,
  feed,
  filepattern,
  lens,
  lib,
  megaparsec,
  notmuch,
  optparse-applicative,
  purebred-email,
  relude,
  say,
  string-interpolate,
  tagsoup,
  text,
  time,
  witch,
}:
mkDerivation {
  pname = "rssfeeds";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base
    containers
    errors
    exceptions
    extra
    feed
    filepattern
    lens
    megaparsec
    notmuch
    optparse-applicative
    purebred-email
    relude
    say
    string-interpolate
    tagsoup
    text
    time
    witch
  ];
  license = "unknown";
}
