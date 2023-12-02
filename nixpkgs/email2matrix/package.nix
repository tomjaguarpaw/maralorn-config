{buildGoModule, fetchFromGitHub, ...}:
buildGoModule rec {
  name = "email2matrix";
  version = "817f819deb96f093394932537faa7f623e0126b9";
  src = fetchFromGitHub {
    owner = "devture";
    repo = "email2matrix";
    rev = version;
    sha256 = "sha256-FyEIBFHGCjFVAIBXru1CNukDwXdRLnwXsKYJb9RVo9k=";
  };
  vendorSha256 = "sha256-keYTRyg5CbYUQFIm75lGJzxIvW3lo8TqiRL3l2Q5Ex0=";
}
