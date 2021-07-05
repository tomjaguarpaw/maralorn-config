{ config, ... }:
let
  nvim-spell-de-utf8-dictionary = builtins.fetchurl {
    url = "http://ftp.vim.org/vim/runtime/spell/de.utf-8.spl";
    sha256 = "1ld3hgv1kpdrl4fjc1wwxgk4v74k8lmbkpi1x7dnr19rldz11ivk";
  };

  nvim-spell-de-utf8-suggestions = builtins.fetchurl {
    url = "http://ftp.vim.org/vim/runtime/spell/de.utf-8.sug";
    sha256 = "0j592ibsias7prm1r3dsz7la04ss5bmsba6l1kv9xn3353wyrl0k";
  };

  nvim-spell-de-latin1-dictionary = builtins.fetchurl {
    url = "http://ftp.vim.org/vim/runtime/spell/de.latin1.spl";
    sha256 = "0hn303snzwmzf6fabfk777cgnpqdvqs4p6py6jjm58hdqgwm9rw9";
  };

  nvim-spell-de-latin1-suggestions = builtins.fetchurl {
    url = "http://ftp.vim.org/vim/runtime/spell/de.latin1.sug";
    sha256 = "0mz07d0a68fhxl9vmy1548vnbayvwv1pc24zhva9klgi84gssgwm";
  };
in
{
  home.file."${config.xdg.configHome}/nvim/spell/de.utf-8.spl".source = nvim-spell-de-utf8-dictionary;
  home.file."${config.xdg.configHome}/nvim/spell/de.utf-8.sug".source = nvim-spell-de-utf8-suggestions;
  home.file."${config.xdg.configHome}/nvim/spell/de.latin1.spl".source = nvim-spell-de-latin1-dictionary;
  home.file."${config.xdg.configHome}/nvim/spell/de.latin1.sug".source = nvim-spell-de-latin1-suggestions;
}
