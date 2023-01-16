let types = ./types.dhall

in  { users = toMap
        { maralorn =
          { filterTag = ""
          , passwordHash =
              "\$argon2id\$v=19\$m=65536,t=2,p=1\$2capLVJQQBvndRxOKIVogQ==\$wCGgmPx4yVyr+nMM9fWCtn1aPvi3uZDvpdhX85GuIxU="
          , userConfig =
            { localBackend =
                types.LocalBackend.TaskwarriorBackend
                  { createHooksOnStart = True
                  , hookListenPort =
                      types.PortConfig.PortRange { min = 40000, max = 50000 }
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
    }
