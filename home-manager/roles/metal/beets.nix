{pkgs, config, ...}:
{
  home.packages = [pkgs.beets];
  xdg.configFile."beets/config.yaml".text = builtins.toJSON {
    directory = config.services.mpd.musicDirectory;
    import.move = true;
    paths = {
      default = "$genre/%the{$albumartist}/$album/%if{$multidisc,Disc $disc/}$track. $title";
      singleton = "$genre/%the{$artist}/singles/$title";
      comp = "$genre/%the{$artist}/$album%/%if{$multidisc,Disc $disc/}$track. $title";
      "genre:soundtrack" = "Soundtrack/$album/%if{$multidisc,Disc $disc/}$track. $title";
    };
    plugins = "convert web fromfilename the duplicates missing inline";
    item_fields.multidisc = "1 if disctotal > 1 else 0";
    convert = {
      auto = true;
      command = "${pkgs.ffmpeg}/bin/ffmpeg -i $source -y -vn -acodec libopus -ab 192k $dest";
      extension = "opus";
      never_convert_lossy_files = true;
    };
  };
}
