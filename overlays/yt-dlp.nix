final: prev: {
  yt-dlp = prev.yt-dlp.overrideAttrs (old: {
    patches = [
      (final.fetchpatch {
        url = "https://github.com/yt-dlp/yt-dlp/commit/149eb0bbf34fa8fdf8d1e2aa28e17479d099e26b.patch";
        hash = "sha256-NQbMUBd1xZWKJaaNmxY7UhXwiPY0Hnf7hxfzDBjAZH8=";
      })

      (final.fetchpatch {
        url = "https://github.com/yt-dlp/yt-dlp/commit/c61cf091a54d3aa3c611722035ccde5ecfe981bb.patch";
        hash = "sha256-ytx/fpeWtIB9KPQ4UsmJuj6AGuB1D7Sei188SJ1LqGY=";
      })
    ];
  });
}
