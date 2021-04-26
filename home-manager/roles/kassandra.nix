{ ... }: {
  home.file = {
    "kassandra-config" = {
      target = ".config/kassandra";
      source = ./kassandra;
      recursive = true;
    };
  };
}
