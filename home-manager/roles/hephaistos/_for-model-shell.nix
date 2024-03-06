((import ./. { }).passthru.__unstable__.self.extend (
  _: _: {
    shellPackages = {
      model = "${./.}/model";
      model-lens = "${./.}/model-lens";
      model-migrations = "${./.}/model-migrations";
      model-aeson = "${./.}/model-aeson";
      model-jsaddle = "${./.}/model-jsaddle";
      model-types = "${./.}/model-types";
      console = "${./.}/console";
      common-backend = "${./.}/common-backend";
    };
  }
)).project.shells.ghc
