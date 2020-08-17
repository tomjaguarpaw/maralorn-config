{ buildGoModule, fetchFromGitHub }:
buildGoModule {
  pname = "neuron-language-server";
  version = "0.1";

  src = fetchFromGitHub {
    owner = "aca";
    repo = "neuron-language-server";
    rev = "master";
    sha256 = "02vj5szqric7zbm8fbw9ngg4dwrgi3nss6a4kv28bqjr1zm3zn4v";
  };

  vendorSha256 = "02dajl4l3c8522ik2hmiq8cx4kj4h2ykx8l7qsal5xznx9pqbs7i";
}
