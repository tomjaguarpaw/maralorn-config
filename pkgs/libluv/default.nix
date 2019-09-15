{ stdenv, lib, fetchFromGitHub, cmake, libuv, luajit, pkgconfig }:

stdenv.mkDerivation rec {
  version = "1.30.1-0";
  pname = "libluv";

  src = fetchFromGitHub {
    owner = "luvit";
    repo = "luv";
    rev = "${version}";
    sha256 = "0j0apc19yn9jgy2iw2z3za9311pk8g72c72rj6xg3a3q1r93rkiy";
    fetchSubmodules = true;
  };

  cmakeFlags = [
    "-DLUA=Off"
    "-DBUILD_MODULE=Off"
    "-DWITH_SHARED_LIBUV=On"
    "-DLUA_BUILD_TYPE=System"
  ];

  nativeBuildInputs = [ cmake pkgconfig libuv luajit ];

  enableParallelBuilding = true;

  doCheck = true;
}
