final: prev: {
  prometheus-folder-size-exporter = final.rustPlatform.buildRustPackage rec {
    pname = "prometheus_folder_size_exporter";
    version = "0.5.0";
    src = final.fetchFromGitHub {
      owner = "MindFlavor";
      repo = pname;
      rev = version;
      sha256 = "sha256-G1oWGiTyftloPxgOuiYFO7hofHdbwPCG4QDXK1t5qWk=";
    };
    cargoSha256 = "sha256:0baxb2fqdywfj9jxmhinm7fqyyn6sdfh6s5vnn7mgwawg00b7h7l";
  };
}
