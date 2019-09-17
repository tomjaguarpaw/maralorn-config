{ stdenv, fetchFromGitHub }:

stdenv.mkDerivation {
  name = "powerlevel10k";
  src = fetchFromGitHub {
    owner = "romkatv";
    repo = "powerlevel10k";
    rev = "2218060b2d8b4ca7edf1299c65871cdf700898e3";
    sha256 = "0m08x0fbj7x800i79nib0km3yx80rrg9f1rh7i24x5kla8k98sgj";
  };

  installPhase = ''
    mkdir -p $out/powerlevel10k
    mv * $out/powerlevel10k
  '';
}
