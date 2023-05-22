final: prev: {
  element-web = final.runCommand "maralorns-element-web" { } ''
    cp -r ${prev.element-web} $out
    cd $out
    chmod u+w -R $out
    bundlecss=$(find . -name 'bundle.css')
    cat ${./user.css} >> $bundlecss
    bundlejs=$(find . -name 'vendors~init.js')
    sed -i 's/return n.room.roomId===e||n.isUnread}/return n.room.roomId===e||n.hasUnreadCount}/' $bundlejs
    cp ${./orville_communicator.opus} media/message.ogg
  '';
}
