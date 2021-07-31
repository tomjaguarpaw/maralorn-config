self: super: {
  gnome = super.gnome.overrideScope' (
    selfg: superg: {
      gnome-shell = superg.gnome-shell.overrideAttrs (
        old: {
          patches = (old.patches or [ ]) ++ [
            (
              self.substituteAll {
                src = ./gnome-shell_3.38.3-3ubuntu1_3.38.3-3ubuntu2.patch;
              }
            )
          ];
        }
      );
    }
  );
}
