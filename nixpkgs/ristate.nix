_final: prev: {
  ristate = prev.ristate.overrideAttrs (drv: rec {
    src = prev.fetchFromGitLab {
      owner = "snakedye";
      repo = "ristate";
      rev = "92e989f26cadac69af1208163733e73b4cf447da";
      hash = "sha256-6slH7R6kbSXQBd7q38oBEbngaCbFv0Tyq34VB1PAfhM=";
    };
    cargoDeps = drv.cargoDeps.overrideAttrs (_: {
      inherit src;
      outputHash = "sha256-fOo9C0dNL9dYy5wXq/yEDqOV0OhOTEY42XK8ShpQh6k=";
    });
  });
}
