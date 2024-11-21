{ pkgs, ... }:
{
  home = {
    packages = builtins.attrValues {
      inherit (pkgs.nodePackages) typescript-language-server;
      inherit (pkgs.python3Packages) python-lsp-server;
      inherit (pkgs)
        rust-analyzer # rust language server
        taplo # toml language server
        texlab # latex language server
        lean # lean language server
        yaml-language-server
        ltex-ls # languagetool support for markdown
        go
        gdb
        shfmt
        astyle
        nodejs
        tailwindcss
        ;
    };
  };
}
