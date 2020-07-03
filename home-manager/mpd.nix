{ pkgs, config, ... }:
let musicDirectory = "${config.home.homeDirectory}/media/audio";
in {
  home.file.".config/beets/config.yaml".text = builtins.toJSON {
    directory = musicDirectory;
    library = "~/.config/beets/musiclibrary.db";
    import = { move = true; };
    paths = {
      default = "$genre/%the{$albumartist}/$album%aunique{}/$track $title";
      singleton = "$genre/%the{$artist}/singles/$title";
      comp = "$genre/%the{$artist}/$album%aunique{}/$track $title";
      "genre:soundtrack" = "Soundtrack/$album%aunique{}/$track $title";
      "genre::classical" =
        "$genre/%the{$composer}/$album%aunique{}/$track $title";
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
  services = {
    mpd = {
      enable = true;
      inherit musicDirectory;
      extraConfig = ''
        audio_output {
              type "pulse"
              name "Pulseaudio"
              server "localhost"
        }
      '';
    };
    mpdris2.enable = true;
  };
}
