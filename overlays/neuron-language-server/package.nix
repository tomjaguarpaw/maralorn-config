{ buildGoModule, fetchFromGitHub }:
buildGoModule {
  pname = "neuron-language-server";
  version = "0.1.1";

  src = fetchFromGitHub {
    owner = "aca";
    repo = "neuron-language-server";
    rev = "450a7cf";
    sha256 = "117njzgiqpnrc0a75d9gq6bvnvv1zb01jjjw87i8s2c3kikjc0sw";
  };

  doCheck = false;

  vendorSha256 = "0qbfkjsmnmvbj59229yv44abzny2k7hfn78gdzs7ddyi3fv02vnm";
}
