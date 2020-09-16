{ buildGoModule, fetchFromGitHub }:
buildGoModule {
  pname = "neuron-language-server";
  version = "0.1.1";

  src = fetchFromGitHub {
    owner = "aca";
    repo = "neuron-language-server";
    rev = "master";
    sha256 = "1kbh0bzzfmk7aj3c6k3ifwx4p42lw2pnr68srk3qpy6hjna8nczb";
  };

  doCheck = false;

  vendorSha256 = "0pjjkw0633l8qbvwzy57rx76zjn3w3kf5f7plxnpxih9zj0q258l";
}
