{ rustPlatform }:
with rustPlatform; buildRustPackage rec {
  name = "rust-scripts";
  src = ./.;
#  depsSha256 = "";
  cargoSha256 = "0h1fimvkm05y5vi2c2baxys5scv2icf0g6bjp62p7x935y5j85ks";
  doCheck = false;
}
