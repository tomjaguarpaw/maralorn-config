_final: prev: {
  element-web = prev.element-web.overrideAttrs (_: {
    postConfigure = ''
      patch node_modules/matrix-react-sdk/lib/components/views/rooms/RoomList.js ${
        ./RoomList.js.patch
      }
      cp ${
        ./orville_communicator.opus
      } node_modules/matrix-react-sdk/res/media/message.ogg
    '';
    preInstall = ''
      find . -name 'bundle.css'
      bundlecss=$(find . -name 'bundle.css')
      cat ${./user.css} >> $bundlecss
    '';
  });
}
