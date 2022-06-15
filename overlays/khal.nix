final: prev: {
  khal = prev.khal.overrideAttrs (old: {
    src = prev.fetchFromGitHub {
      rev = "a270656b4754329a0e76e72a473307548c18afb8";
      owner = "pimutils";
      repo = "khal";
      hash = "sha256-LwwaCGl4NFG4dJuvr75ZfFZMX2ivwLg1Z8eQcjaLtwI=";
    };
    SETUPTOOLS_SCM_PRETEND_VERSION = "unstable-2022-06-14";
    doInstallCheck = false;
  });
}
