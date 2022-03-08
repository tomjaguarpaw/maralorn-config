{
  mkDerivation,
  aeson,
  ansi-wl-pprint,
  async,
  base,
  bytestring,
  containers,
  directory,
  exceptions,
  extra,
  fetchzip,
  filepath,
  hashable,
  haskeline,
  hoogle,
  hpack,
  html-conduit,
  http-client,
  http-client-tls,
  http-types,
  lib,
  mtl,
  network-uri,
  optparse-applicative,
  process,
  temporary,
  terminal-size,
  text,
  time,
  transformers,
  xml-conduit,
}:
mkDerivation {
  pname = "haskell-docs-cli";
  version = "1.0.0.0";
  src = fetchzip {
    url = "https://github.com/lazamar/haskell-docs-cli/archive/e7f1a60db8696fc96987a3447d402c4d0d54b5e0.zip";
    sha256 = "0n784lr4jqx8i2a9jhbcrmb5swvsggrz6viwbv73y1qvb1f67mgz";
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
  libraryToolDepends = [hpack];
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
