self: super:
let
  package = { python3Packages, fetchFromGitHub }:
    let
      pname = "matrix-commander";
      version = "67a6a89";
    in python3Packages.buildPythonApplication {
      name = "${pname}-${version}";
      inherit pname version;
      src = fetchFromGitHub {
        owner = "8go";
        repo = "matrix-commander";
        rev = version;
        sha256 = "0k387a0i9jh6034f8yy3b8wxsjr8abb896rfmmbmh5gx1a6f5cz4";
      };
      format = "other";
      installPhase = ''
        mkdir -p $out/bin
        install matrix-commander.py $out/bin/matrix-commander
      '';
      propagatedBuildInputs = with python3Packages; [
        aiohttp
        aiofiles
        markdown
        matrix-nio
        notify2
        pillow
        python_magic
      ];
      checkPhase = ''
        $out/bin/matrix-commander --help > /dev/null
      '';
    };
in { matrix-commander = self.callPackage package { }; }
