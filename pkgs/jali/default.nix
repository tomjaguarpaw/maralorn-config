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
  version = "3a884c7eb3b294c5aaf2191408e1ac2befbceaaf";
  src = fetchgit {
    url = "https://git.darmstadt.ccc.de/jali/jali.git";
    rev = version;
    sha256 = "0fkb4vyf570d7zcwi3mix9g44wqpy128gd3li0grkbml7aflzxfr";
  };
  propagatedBuildInputs = [ jinja2 pendulum GitPython andir.aqbanking ];
}
