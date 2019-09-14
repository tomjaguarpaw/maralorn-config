{ stdenv, fetchFromGitHub }:

stdenv.mkDerivation {
  name = "powerlevel10k";
  src = fetchFromGitHub {
    owner = "romkatv";
    repo = "powerlevel10k";
    rev = "b451b7a75821afedba6e9fa3c422e8eca9f8dedc";
    sha256 = "0gb27ml1ypi3j5xilvv38b73agqcf2y6ll532zkcv4lwha3ldhmy";
  };

  installPhase = ''
    mkdir -p $out/powerlevel10k
    mv * $out/powerlevel10k
  '';
}
