{ buildGoModule, fetchFromGitHub }:
buildGoModule {
  pname = "neuron-language-server";
  version = "0.1";

  src = fetchFromGitHub {
    owner = "aca";
    repo = "neuron-language-server";
    rev = "master";
    sha256 = "1kbh0bzzfmk7aj3c6k3ifwx4p42lw2pnr68srk3qpy6hjna8nczb";
  };

  vendorSha256 = "02dajl4l3c8522ik2hmiq8cx4kj4h2ykx8l7qsal5xznx9pqbs7i";
}
