{ aqbanking, python36Packages, fetchgit }:
with python36Packages;
buildPythonApplication rec {
  name = "${pname}-${version}";
  pname = "jali";
  doCheck = false;
  version = "2c1dfa1a";
  src = fetchgit {
    url = "https://git.darmstadt.ccc.de/jali/jali.git";
    rev = version;
    sha256 = "1xac5rs848nyxshgrv52i1w1y8bgbawv7spr3489p8f22crvawra";
  };
  #src = /home/maralorn/git/jali;
  propagatedBuildInputs = [ jinja2 pendulum GitPython aqbanking ];
}
