((import ./. { }).passthru.__unstable__.self.extend (
  _: _: {
    shellPackages = {
      model = "${./.}/model";
      console = "${./.}/console";
      common-backend = "${./.}/common-backend";
    };
  }
)).project.shells.ghc
