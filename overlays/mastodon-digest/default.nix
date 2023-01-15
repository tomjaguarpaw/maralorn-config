final: prev: let
  pkgs = final;
  deps = ps:
    with ps; [
      jinja2
      mastodon-py
      scipy
      python-dotenv
    ];
  python-env = pkgs.python3.withPackages deps;
  src = pkgs.fetchFromGitHub {
    owner = "hodgesmr";
    repo = "mastodon_digest";
    rev = "07d8827a79086263f0e0f161faa2c12e405b2929";
    hash = "sha256-iFOvexzj5UQve67nvNcGthrvVbL5yQbYyUIXlaRYlug=";
  };
  patchedSrc = pkgs.runCommand "mastodon_digest-patched-src" ''    { }
       cp -r ${src} $out
       chmod -R +w $out
       patch -d $out -p1 < ${./all-posts.patch}
  '';
in {
  mastodon_digest = pkgs.writeShellApplication {
    name = "mastodon_digest";
    runtimeInputs = [python-env];
    text = ''
      set -o allexport
      # shellcheck source=/dev/null
      source .env
      set +o allexport
      cd ${patchedSrc} && python run.py "''${@}"
    '';
  };
}
