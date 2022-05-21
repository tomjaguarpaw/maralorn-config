final: prev: let
  chime = final.fetchurl {
    url = "https://www.mediacollege.com/downloads/sound-effects/star-trek/tng/tng-doorbell.ogg";
    sha256 = "sha256-tpDJ8lnmSwG5Puq6dJhiW3w+Id0EiPoqlim6N3BPz7c=";
  };
in {
  element-web = prev.element-web.overrideAttrs (old: {
    preInstall = ''
      bundle=$(find . -name 'bundle.css')
      cat ${./user.css} >> $bundle
      cp ${chime} media/message.ogg
    '';
  });
}
