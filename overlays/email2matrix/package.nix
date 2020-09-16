{ buildGoModule, fetchFromGitHub, ... }:
buildGoModule rec {
  name = "email2matrix";
  version = "782bcfdd67983ff27f0b9cec5c81cf1a20796ab0";
  src = fetchFromGitHub {
    owner = "devture";
    repo = "email2matrix";
    rev = version;
    sha256 = "0nx99iab2y10m4jh4jl9c4y7j4iy8zlyfcn42v4y4mlk1507czlj";
  };
  vendorSha256 = "0nrl1d1628isd6183a9rj4qmsmzpbsf656cm75vw0lz2x0s4x7dg";
}
