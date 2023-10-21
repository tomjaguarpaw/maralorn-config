let types = ./types.dhall

in  { backends =
      [ { name = "default local backend"
        , backend =
            types.StandaloneAccount.LocalAccount
              { userConfig =
                { localBackend =
                    types.LocalBackend.TaskwarriorBackend
                      { createHooksOnStart = True
                      , hookListenPort =
                          types.PortConfig.PortRange
                            { min = 40000, max = 50000 }
                      , hookSuffix = "kassandra"
                      , removeHooksOnExit = True
                      , taskBin = None Text
                      , taskConfig = [] : List types.TaskwarriorOption
                      , taskDataPath = None Text
                      , taskRcPath = None Text
                      }
                , uiConfig = ./uiConfig.dhall
                }
              }
        }
      , { name = "Dynamic Backend"
        , backend =
            types.StandaloneAccount.RemoteAccount
              { backend = None types.RemoteBackend }
        }
      ]
    }
