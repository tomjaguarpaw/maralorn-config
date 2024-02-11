_final: prev:
let
  set =
    n:
    prev.${n}.overrideAttrs (old: {
      meta = old.meta // {
        mainProgram = n;
      };
    });
in
{
  newsboat = set "newsboat";
  rbw = set "rbw";
  sd = set "sd";
}
