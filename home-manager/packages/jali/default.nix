{ aqbanking, python36Packages, fetchgit }:
with python36Packages; buildPythonApplication rec {
  name = "${pname}-${version}";
  pname = "jali";
  doCheck = false;
  version = "1d1c5d0a";
  src = fetchgit {
    url = "https://git.darmstadt.ccc.de/jali/jali.git";
    rev = version;
    sha256 = "1nzzangp7yr2gq66qz7wk2cqqwjlhrfaqmc85qigjv4vpfmlphl0";
  };
  propagatedBuildInputs = [ jinja2 pendulum GitPython aqbanking ];
}
