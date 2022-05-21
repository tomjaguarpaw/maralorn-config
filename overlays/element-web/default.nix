final: prev: let
  chime = final.fetchurl {
    url = "https://www.mediacollege.com/downloads/sound-effects/star-trek/tng/tng-doorbell.ogg";
    sha256 = "sha256-tpDJ8lnmSwG5Puq6dJhiW3w+Id0EiPoqlim6N3BPz7c=";
  };
in {
  element-web = prev.element-web.overrideAttrs (old: {
    preInstall = ''
      bundlecss=$(find . -name 'bundle.css')
      cat ${./user.css} >> $bundlecss
      bundlejs=$(find . -name 'vendors~init.js')
      sed -i 's/return n.room.roomId===e||n.isUnread}/return n.room.roomId===e||n.hasUnreadCount}/' $bundlejs
      cp ${chime} media/message.ogg
    '';
  });
}
