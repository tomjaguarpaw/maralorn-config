final: _prev:
let
  pkgs = final;
  deps =
    p:
    builtins.attrValues {
      inherit (p)
        jinja2
        mastodon-py
        scipy
        python-dotenv
        ;
    };
  python-env = pkgs.python3.withPackages deps;
  src = pkgs.fetchFromGitHub {
    owner = "hodgesmr";
    repo = "mastodon_digest";
    rev = "07d8827a79086263f0e0f161faa2c12e405b2929";
    hash = "sha256-iFOvexzj5UQve67nvNcGthrvVbL5yQbYyUIXlaRYlug=";
  };
  patchedSrc = pkgs.runCommand "mastodon_digest-patched-src" {} ''
    cp -r ${src} $out
    chmod -R +w $out
    patch -d $out -p1 < ${./all-posts.patch}
    cd $out/templates/themes
    cp -r default no-boosts
    cp ${./index.html.jinja} no-boosts/index.html.jinja
  '';
in
{
  mastodon_digest = pkgs.writeShellApplication {
    name = "mastodon_digest";
    runtimeInputs = [python-env];
    text = ''
      # shellcheck source=/dev/null
      cd ${patchedSrc} && python run.py "''${@}"
    '';
  };
}
