final: prev: {
  element-web = prev.element-web.overrideAttrs (old: {
    preInstall = ''
      bundle=$(find . -name 'bundle.css')
      cat ${./user.css} >> $bundle
    '';
  });
}
