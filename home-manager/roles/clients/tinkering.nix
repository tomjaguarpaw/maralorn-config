{ pkgs, ... }:
{
  home = {
    packages = builtins.attrValues {
      inherit (pkgs)
        meld
        gparted
        httpie
        tea
        ;
    };
    file.".editorconfig".text = ''
      # Top-most EditorConfig file
      root = true

      # Unix-style newlines with a newline ending every file, utf-8 charset
      [*]
      indent_size = 3
      end_of_line = lf
      insert_final_newline = true
      trim_trailing_whitespace = true
      charset = utf-8
    '';
    file = {
      ".cabal/config".text = ''
        repository hackage.haskell.org
          url: http://hackage.haskell.org/

        username: maralorn
        password-command: rbw get hackage.haskell.org
      '';
    };
  };
}
