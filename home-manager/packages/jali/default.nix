{ aqbanking, python36Packages, fetchgit }:
with python36Packages; buildPythonApplication rec {
  name = "${pname}-${version}";
  pname = "jali";
  doCheck = false;
  version = "b97317f";
  src = fetchgit {
    url = "https://git.darmstadt.ccc.de/jali/jali.git";
    rev = version;
    sha256 = "1sj9hmjij0b3jzj2hkny3isn1ni8q3d9bmmasswxwwjcvn4g6nk4";
  };
  propagatedBuildInputs = [ jinja2 pendulum GitPython aqbanking ];
}
