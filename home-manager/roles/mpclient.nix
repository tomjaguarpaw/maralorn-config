{pkgs, ...}: {
  home.packages = [pkgs.ncmpcpp];
  home.file.".ncmpcpp/config".text = ''
    ask_before_clearing_playlists=no
    mouse_support = yes
    song_columns_list_format = "(24)[red]{a} $R(48)[blue]{t} (24)[green]{b} (4)[magenta]{l}"
    playlist_display_mode = columns
    search_engine_display_mode = columns
    browser_display_mode = columns
    user_interface = alternative
  '';
}
