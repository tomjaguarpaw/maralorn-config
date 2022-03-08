{
  lib,
  stdenv,
  fetchFromGitHub,
  pkg-config,
  qt5,
  avahi,
  boost,
  libopus,
  libsndfile,
  protobuf,
  speex,
  libcap,
  alsaLib,
  python3,
  cmake,
  poco,
  pcre,
  iceSupport ? false,
  zeroc-ice,
  rnnoise,
  jackSupport ? false,
  libjack2,
  pulseSupport ? false,
  libpulseaudio,
  grpcSupport ? false,
  grpc,
  which,
  speechdSupport ? false,
  speechd,
  nixosTests,
}: let
  generic = overrides: source:
    qt5.mkDerivation (source
    // overrides
    // {
      pname = overrides.type;
      version = source.version;

      patches = (source.patches or []);
      nativeBuildInputs =
        [pkg-config python3 cmake]
        ++ (overrides.nativeBuildInputs or []);

      buildInputs =
        [boost protobuf avahi]
        ++ (overrides.buildInputs or []);

      passthru.tests.connectivity = nixosTests.mumble;

      meta = with lib; {
        description = "Low-latency, high quality voice chat software";
        homepage = "https://mumble.info";
        license = licenses.bsd3;
        maintainers = with maintainers; [petabyteboy infinisil];
        platforms = platforms.linux;
      };
    });

  client = source:
    generic
    {
      type = "mumble";

      nativeBuildInputs = [
        qt5.qttools
      ];

      buildInputs =
        [
          libopus
          libsndfile
          speex
          qt5.qtsvg
          rnnoise
          poco
          pcre
          speechd
          zeroc-ice.all
          libpulseaudio
        ]
        ++ lib.optional stdenv.isLinux alsaLib
        ++ lib.optional jackSupport libjack2;

      configureFlags = [
        "CONFIG+=no-server"
      ];

      NIX_CFLAGS_COMPILE = "-w";

      cmakeFlags = [
        "-Dice=off"
        "-Dpulseaudio=on"
        "-Dalsa=on"
        "-Doverlay-xcompile=off"
        "-DRELEASE_ID=${source.version}"
      ];

      installPhase = ''
        # bin stuff
        install -Dm755 mumble $out/bin/mumble
        wrapProgram $out/bin/mumble \
          --prefix LD_LIBRARY_PATH : "${libpulseaudio}/lib"
        install -Dm755 $src/scripts/mumble-overlay $out/bin/mumble-overlay

        # lib stuff
        mkdir -p $out/lib/mumble
        cp -P libcelt* $out/lib/mumble
        cp -rP plugins/* $out/lib/mumble

        # icons
        install -Dm644 $src/scripts/org.mumble_voip.mumble.desktop $out/share/applications/mumble.desktop
        install -Dm644 $src/icons/mumble.svg $out/share/icons/hicolor/scalable/apps/mumble.svg
      '';
    }
    source;

  server = source:
    generic
    {
      type = "murmur";

      configureFlags = [
        "CONFIG+=no-client"
      ];

      buildInputs =
        [libcap]
        ++ lib.optionals grpcSupport [grpc which];

      installPhase = ''
        # bin stuff
        install -Dm755 release/murmurd $out/bin/murmurd
      '';
    }
    source;

  source = rec {
    version = "1.4.0-development-snapshot-005";

    # Needs submodules
    src = fetchFromGitHub {
      owner = "mumble-voip";
      repo = "mumble";
      rev = "${version}";
      sha256 = "sha256:1w16j5idbvvbbisj1jlk5igym2n2f67ia61yynibqbmpvyq0bg3z";
      fetchSubmodules = true;
    };
  };
in {
  mumble = client source;
  murmur = server source;
}
