{ mkDerivation
, aeson
, ansi-wl-pprint
, async
, base
, bytestring
, containers
, directory
, exceptions
, extra
, fetchzip
, filepath
, hashable
, haskeline
, hoogle
, hpack
, html-conduit
, http-client
, http-client-tls
, http-types
, lib
, mtl
, network-uri
, optparse-applicative
, process
, temporary
, terminal-size
, text
, time
, transformers
, xml-conduit
}:
mkDerivation {
  pname = "haskell-docs-cli";
  version = "1.0.0.0";
  src = fetchzip {
    url = "https://github.com/lazamar/haskell-docs-cli/archive/refs/heads/main.tar.gz";
    sha256 = "0zfvh7bfvrvqw6y2galf410cfrk782psr5mfkl0pl8lpgzksa5kw";
  };
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson
    ansi-wl-pprint
    async
    base
    bytestring
    containers
    directory
    exceptions
    extra
    filepath
    hashable
    haskeline
    hoogle
    html-conduit
    http-client
    http-client-tls
    http-types
    mtl
    network-uri
    optparse-applicative
    process
    temporary
    terminal-size
    text
    time
    transformers
    xml-conduit
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    aeson
    ansi-wl-pprint
    async
    base
    bytestring
    containers
    directory
    exceptions
    extra
    filepath
    hashable
    haskeline
    hoogle
    html-conduit
    http-client
    http-client-tls
    http-types
    mtl
    network-uri
    optparse-applicative
    process
    temporary
    terminal-size
    text
    time
    transformers
    xml-conduit
  ];
  testHaskellDepends = [
    aeson
    ansi-wl-pprint
    async
    base
    bytestring
    containers
    directory
    exceptions
    extra
    filepath
    hashable
    haskeline
    hoogle
    html-conduit
    http-client
    http-client-tls
    http-types
    mtl
    network-uri
    optparse-applicative
    process
    temporary
    terminal-size
    text
    time
    transformers
    xml-conduit
  ];
  prePatch = "hpack";
  homepage = "https://github.com/githubuser/haskell-docs-cli#readme";
  description = "Search Hoogle and navigate Hackage from the command line";
  license = lib.licenses.bsd3;
}
