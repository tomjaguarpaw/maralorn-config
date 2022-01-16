final: prev: {
  laminar = prev.laminar.overrideAttrs (old: {
    patches = (old.patches or [ ]) ++ [
      (prev.fetchpatch {
        url = "https://github.com/ohwgiles/laminar/commit/549f49052a31863fdf7fe6e299e1874d66d6dc57.patch";
        sha256 = "sha256-U4b7SFt4qcBqYvMdaXpRrlD4+mIjqlPzo7UALvM5bPc=";
      })
      (prev.fetchpatch {
        url = "https://github.com/ohwgiles/laminar/commit/37bbf6ade49a01eb8feeb47b3aba254aea28ade4.patch";
        sha256 = "sha256-rP2wwkiO78neuryLnTD+eooBqCqfBeKVqL2DyEZDjok=";
      })
    ];
  });
}
