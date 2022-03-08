self: super: let
  path = "PATH : ${super.lib.makeBinPath [super.imagemagick super.pdftk]}";
in {
  falsisign = super.stdenvNoCC.mkDerivation rec {
    pname = "falsisign";
    version = "8e7114b";
    src = super.fetchFromGitLab {
      owner = "edouardklein";
      repo = "falsisign";
      rev = version;
      hash = "sha256-UJH8m1kWbW6m+6CyTfoF+oDO36c82kECHIUFaF9cT4U=";
    };
    buildInputs = [super.makeWrapper];
    dontBuild = true;
    installPhase = ''
      patchShebangs *.sh
      mkdir -p $out/bin
      cp falsisign.sh $out/bin/falsisign
      cp signdiv.sh $out/bin/signdiv
      wrapProgram $out/bin/falsisign --prefix ${path}
      wrapProgram $out/bin/signdiv --prefix ${path}
    '';
  };
}
