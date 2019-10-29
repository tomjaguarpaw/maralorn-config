{ aqbanking, python36Packages, fetchgit }:
let
  andir = import (builtins.fetchTarball {
    url = "https://github.com/andir/nixpkgs/archive/aqbanking.tar.gz";
    sha256 = "06m7risi68jfqgv001ip77lzz6bylvvdn5j8sn845c54zpm6qp36";
  }) { };
in with python36Packages;
buildPythonApplication rec {
  name = "${pname}-${version}";
  pname = "jali";
  doCheck = false;
  version = "b47d3b9";
  src = fetchgit {
    url = "https://git.darmstadt.ccc.de/jali/jali.git";
    rev = version;
    sha256 = "0l5h9hjri77zifx3x4khw7ncmmc9l9ppisdjilsfllzkabz4xjf4";
  };
  propagatedBuildInputs = [ jinja2 pendulum GitPython andir.aqbanking ];
}
