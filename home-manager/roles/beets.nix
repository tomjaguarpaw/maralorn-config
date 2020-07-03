{ pkgs, config, ... }: {
  home.file.".config/beets/config.yaml".text = builtins.toJSON {
    directory = config.services.mpd.musicDirectory;
    import.move = true;
    paths = {
      default = "$genre/%the{$albumartist}/$album%aunique{}/$track. $title";
      singleton = "$genre/%the{$artist}/singles/$title";
      comp = "$genre/%the{$artist}/$album%aunique{}/$track. $title";
      "genre:soundtrack" = "Soundtrack/$album%aunique{}/$track. $title";
      "genre::classical" =
        "$genre/%the{$composer}/$album%aunique{}/$track. $title";
    };
    plugins =
      "convert web mpdstats mpdupdate fromfilename the duplicates missing";
    convert = {
      auto = true;
      command =
        "${pkgs.ffmpeg}/bin/ffmpeg -i $source -y -vn -acodec libopus -ab 128k $dest";
      extension = "opus";
      never_convert_lossy_files = true;
    };
  };
  systemd.user.services.beets-mpdstats = {
    Unit.Description = "beets.io mpdstats recorder";
    Install.WantedBy = [ "default.target" ];
    Service = {
      ExecStart = "${pkgs.beets}/bin/beet mpdstats";
      Restart = "always";
    };
  };
}
