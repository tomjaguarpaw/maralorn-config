{
  aqbanking,
  python3Packages,
  fetchgit,
}: let
  inherit (python3Packages) jinja2 pendulum GitPython buildPythonApplication;
  pname = "jali";
  version = "b47d3b9";
in
  buildPythonApplication {
    name = "${pname}-${version}";
    inherit pname version;
    doCheck = false;
    src = fetchgit {
      url = "https://git.darmstadt.ccc.de/jali/jali.git";
      rev = version;
      sha256 = "0l5h9hjri77zifx3x4khw7ncmmc9l9ppisdjilsfllzkabz4xjf4";
    };
    propagatedBuildInputs = [jinja2 pendulum GitPython aqbanking];
  }
