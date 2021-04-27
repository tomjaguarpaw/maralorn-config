{
StandaloneAccount = < RemoteAccount :
    { backend :
        Optional
          { url : Text
          , user : Text
          , password :
              < Prompt
              | Password : { plaintext : Text }
              | PasswordCommand : { command : Text }
              >
          }
    }
| LocalAccount :
    { userConfig :
        { localBackend :
            < TaskwarriorBackend :
                { taskRcPath : Optional Text
                , taskDataPath : Optional Text
                , taskConfig : List { name : Text, value : Text }
                , taskBin : Optional Text
                , hookListenPort :
                    < Port : { port : Natural }
                    | PortRange : { min : Natural, max : Natural }
                    >
                , hookSuffix : Text
                , createHooksOnStart : Bool
                , removeHooksOnExit : Bool
                }
            | GitBackend :
                { directoryPath : Text
                , commit : Bool
                , configureMerge : Bool
                , createIfMissing : Bool
                , origin : Optional Text
                , pushOnWrite : Bool
                , watchFiles : Bool
                , pullTimerSeconds : Optional Natural
                }
            >
        , uiConfig :
            { viewList :
                List
                  < SearchWidget
                  | ConfigListWidget : { name : Text, limit : Optional Natural }
                  >
            , configuredLists :
                List
                  { name : Text
                  , list :
                      < QueryList :
                          { query :
                              List
                                < HasProperty :
                                    { property :
                                        < DescriptionMatches : { filter : Text }
                                        | ParentBlocked
                                        | Blocked
                                        | Waiting
                                        | Pending
                                        | Completed
                                        | Deleted
                                        | IsParent
                                        | OnList
                                        | HasTag : { tag : Text }
                                        | HasParent
                                        >
                                    }
                                | HasntProperty :
                                    { property :
                                        < DescriptionMatches : { filter : Text }
                                        | ParentBlocked
                                        | Blocked
                                        | Waiting
                                        | Pending
                                        | Completed
                                        | Deleted
                                        | IsParent
                                        | OnList
                                        | HasTag : { tag : Text }
                                        | HasParent
                                        >
                                    }
                                >
                          }
                      | TagList : { name : Text }
                      | DefinitionList :
                          { elements :
                              List
                                < ConfigList :
                                    { name : Text, limit : Optional Natural }
                                | ListElement :
                                    { item :
                                        < TaskwarriorTask : { uuid : Text }
                                        | AdHocTask : { description : Text }
                                        | HabiticaTask :
                                            { task :
                                                < HabiticaDaily | HabiticaTodo >
                                            }
                                        | Mail : { id : Text }
                                        >
                                    }
                                >
                          }
                      | ChildrenList : { uuid : Text }
                      | DependenciesList : { uuid : Text }
                      | HabiticaList :
                          { list : < HabiticaDailys | HabiticaTodos > }
                      | Mails
                      >
                  }
            , uiFeatures :
                { sortInTag : Bool
                , treeOption : < NoTree | PartOfTree | DependsTree >
                }
            }
        }
    }
>,
BackendConfig = { users :
    List
      { mapKey : Text
      , mapValue :
          { passwordHash : Text
          , userConfig :
              { localBackend :
                  < TaskwarriorBackend :
                      { taskRcPath : Optional Text
                      , taskDataPath : Optional Text
                      , taskConfig : List { name : Text, value : Text }
                      , taskBin : Optional Text
                      , hookListenPort :
                          < Port : { port : Natural }
                          | PortRange : { min : Natural, max : Natural }
                          >
                      , hookSuffix : Text
                      , createHooksOnStart : Bool
                      , removeHooksOnExit : Bool
                      }
                  | GitBackend :
                      { directoryPath : Text
                      , commit : Bool
                      , configureMerge : Bool
                      , createIfMissing : Bool
                      , origin : Optional Text
                      , pushOnWrite : Bool
                      , watchFiles : Bool
                      , pullTimerSeconds : Optional Natural
                      }
                  >
              , uiConfig :
                  { viewList :
                      List
                        < SearchWidget
                        | ConfigListWidget :
                            { name : Text, limit : Optional Natural }
                        >
                  , configuredLists :
                      List
                        { name : Text
                        , list :
                            < QueryList :
                                { query :
                                    List
                                      < HasProperty :
                                          { property :
                                              < DescriptionMatches :
                                                  { filter : Text }
                                              | ParentBlocked
                                              | Blocked
                                              | Waiting
                                              | Pending
                                              | Completed
                                              | Deleted
                                              | IsParent
                                              | OnList
                                              | HasTag : { tag : Text }
                                              | HasParent
                                              >
                                          }
                                      | HasntProperty :
                                          { property :
                                              < DescriptionMatches :
                                                  { filter : Text }
                                              | ParentBlocked
                                              | Blocked
                                              | Waiting
                                              | Pending
                                              | Completed
                                              | Deleted
                                              | IsParent
                                              | OnList
                                              | HasTag : { tag : Text }
                                              | HasParent
                                              >
                                          }
                                      >
                                }
                            | TagList : { name : Text }
                            | DefinitionList :
                                { elements :
                                    List
                                      < ConfigList :
                                          { name : Text
                                          , limit : Optional Natural
                                          }
                                      | ListElement :
                                          { item :
                                              < TaskwarriorTask :
                                                  { uuid : Text }
                                              | AdHocTask :
                                                  { description : Text }
                                              | HabiticaTask :
                                                  { task :
                                                      < HabiticaDaily
                                                      | HabiticaTodo
                                                      >
                                                  }
                                              | Mail : { id : Text }
                                              >
                                          }
                                      >
                                }
                            | ChildrenList : { uuid : Text }
                            | DependenciesList : { uuid : Text }
                            | HabiticaList :
                                { list : < HabiticaDailys | HabiticaTodos > }
                            | Mails
                            >
                        }
                  , uiFeatures :
                      { sortInTag : Bool
                      , treeOption : < NoTree | PartOfTree | DependsTree >
                      }
                  }
              }
          , filterTag : Text
          }
      }
},
Widget = < SearchWidget | ConfigListWidget : { name : Text, limit : Optional Natural } >,
NamedListQuery = { name : Text
, list :
    < QueryList :
        { query :
            List
              < HasProperty :
                  { property :
                      < DescriptionMatches : { filter : Text }
                      | ParentBlocked
                      | Blocked
                      | Waiting
                      | Pending
                      | Completed
                      | Deleted
                      | IsParent
                      | OnList
                      | HasTag : { tag : Text }
                      | HasParent
                      >
                  }
              | HasntProperty :
                  { property :
                      < DescriptionMatches : { filter : Text }
                      | ParentBlocked
                      | Blocked
                      | Waiting
                      | Pending
                      | Completed
                      | Deleted
                      | IsParent
                      | OnList
                      | HasTag : { tag : Text }
                      | HasParent
                      >
                  }
              >
        }
    | TagList : { name : Text }
    | DefinitionList :
        { elements :
            List
              < ConfigList : { name : Text, limit : Optional Natural }
              | ListElement :
                  { item :
                      < TaskwarriorTask : { uuid : Text }
                      | AdHocTask : { description : Text }
                      | HabiticaTask :
                          { task : < HabiticaDaily | HabiticaTodo > }
                      | Mail : { id : Text }
                      >
                  }
              >
        }
    | ChildrenList : { uuid : Text }
    | DependenciesList : { uuid : Text }
    | HabiticaList : { list : < HabiticaDailys | HabiticaTodos > }
    | Mails
    >
},
LocalBackend = < TaskwarriorBackend :
    { taskRcPath : Optional Text
    , taskDataPath : Optional Text
    , taskConfig : List { name : Text, value : Text }
    , taskBin : Optional Text
    , hookListenPort :
        < Port : { port : Natural }
        | PortRange : { min : Natural, max : Natural }
        >
    , hookSuffix : Text
    , createHooksOnStart : Bool
    , removeHooksOnExit : Bool
    }
| GitBackend :
    { directoryPath : Text
    , commit : Bool
    , configureMerge : Bool
    , createIfMissing : Bool
    , origin : Optional Text
    , pushOnWrite : Bool
    , watchFiles : Bool
    , pullTimerSeconds : Optional Natural
    }
>,
TreeOption = < NoTree | PartOfTree | DependsTree >,
PortConfig = < Port : { port : Natural } | PortRange : { min : Natural, max : Natural } >,
TaskwarriorOption = { name : Text, value : Text },
StandaloneConfig = { backends :
    List
      { name : Text
      , backend :
          < RemoteAccount :
              { backend :
                  Optional
                    { url : Text
                    , user : Text
                    , password :
                        < Prompt
                        | Password : { plaintext : Text }
                        | PasswordCommand : { command : Text }
                        >
                    }
              }
          | LocalAccount :
              { userConfig :
                  { localBackend :
                      < TaskwarriorBackend :
                          { taskRcPath : Optional Text
                          , taskDataPath : Optional Text
                          , taskConfig : List { name : Text, value : Text }
                          , taskBin : Optional Text
                          , hookListenPort :
                              < Port : { port : Natural }
                              | PortRange : { min : Natural, max : Natural }
                              >
                          , hookSuffix : Text
                          , createHooksOnStart : Bool
                          , removeHooksOnExit : Bool
                          }
                      | GitBackend :
                          { directoryPath : Text
                          , commit : Bool
                          , configureMerge : Bool
                          , createIfMissing : Bool
                          , origin : Optional Text
                          , pushOnWrite : Bool
                          , watchFiles : Bool
                          , pullTimerSeconds : Optional Natural
                          }
                      >
                  , uiConfig :
                      { viewList :
                          List
                            < SearchWidget
                            | ConfigListWidget :
                                { name : Text, limit : Optional Natural }
                            >
                      , configuredLists :
                          List
                            { name : Text
                            , list :
                                < QueryList :
                                    { query :
                                        List
                                          < HasProperty :
                                              { property :
                                                  < DescriptionMatches :
                                                      { filter : Text }
                                                  | ParentBlocked
                                                  | Blocked
                                                  | Waiting
                                                  | Pending
                                                  | Completed
                                                  | Deleted
                                                  | IsParent
                                                  | OnList
                                                  | HasTag : { tag : Text }
                                                  | HasParent
                                                  >
                                              }
                                          | HasntProperty :
                                              { property :
                                                  < DescriptionMatches :
                                                      { filter : Text }
                                                  | ParentBlocked
                                                  | Blocked
                                                  | Waiting
                                                  | Pending
                                                  | Completed
                                                  | Deleted
                                                  | IsParent
                                                  | OnList
                                                  | HasTag : { tag : Text }
                                                  | HasParent
                                                  >
                                              }
                                          >
                                    }
                                | TagList : { name : Text }
                                | DefinitionList :
                                    { elements :
                                        List
                                          < ConfigList :
                                              { name : Text
                                              , limit : Optional Natural
                                              }
                                          | ListElement :
                                              { item :
                                                  < TaskwarriorTask :
                                                      { uuid : Text }
                                                  | AdHocTask :
                                                      { description : Text }
                                                  | HabiticaTask :
                                                      { task :
                                                          < HabiticaDaily
                                                          | HabiticaTodo
                                                          >
                                                      }
                                                  | Mail : { id : Text }
                                                  >
                                              }
                                          >
                                    }
                                | ChildrenList : { uuid : Text }
                                | DependenciesList : { uuid : Text }
                                | HabiticaList :
                                    { list : < HabiticaDailys | HabiticaTodos >
                                    }
                                | Mails
                                >
                            }
                      , uiFeatures :
                          { sortInTag : Bool
                          , treeOption : < NoTree | PartOfTree | DependsTree >
                          }
                      }
                  }
              }
          >
      }
},
PasswordConfig = < Prompt
| Password : { plaintext : Text }
| PasswordCommand : { command : Text }
>,
AccountConfig = { passwordHash : Text
, userConfig :
    { localBackend :
        < TaskwarriorBackend :
            { taskRcPath : Optional Text
            , taskDataPath : Optional Text
            , taskConfig : List { name : Text, value : Text }
            , taskBin : Optional Text
            , hookListenPort :
                < Port : { port : Natural }
                | PortRange : { min : Natural, max : Natural }
                >
            , hookSuffix : Text
            , createHooksOnStart : Bool
            , removeHooksOnExit : Bool
            }
        | GitBackend :
            { directoryPath : Text
            , commit : Bool
            , configureMerge : Bool
            , createIfMissing : Bool
            , origin : Optional Text
            , pushOnWrite : Bool
            , watchFiles : Bool
            , pullTimerSeconds : Optional Natural
            }
        >
    , uiConfig :
        { viewList :
            List
              < SearchWidget
              | ConfigListWidget : { name : Text, limit : Optional Natural }
              >
        , configuredLists :
            List
              { name : Text
              , list :
                  < QueryList :
                      { query :
                          List
                            < HasProperty :
                                { property :
                                    < DescriptionMatches : { filter : Text }
                                    | ParentBlocked
                                    | Blocked
                                    | Waiting
                                    | Pending
                                    | Completed
                                    | Deleted
                                    | IsParent
                                    | OnList
                                    | HasTag : { tag : Text }
                                    | HasParent
                                    >
                                }
                            | HasntProperty :
                                { property :
                                    < DescriptionMatches : { filter : Text }
                                    | ParentBlocked
                                    | Blocked
                                    | Waiting
                                    | Pending
                                    | Completed
                                    | Deleted
                                    | IsParent
                                    | OnList
                                    | HasTag : { tag : Text }
                                    | HasParent
                                    >
                                }
                            >
                      }
                  | TagList : { name : Text }
                  | DefinitionList :
                      { elements :
                          List
                            < ConfigList :
                                { name : Text, limit : Optional Natural }
                            | ListElement :
                                { item :
                                    < TaskwarriorTask : { uuid : Text }
                                    | AdHocTask : { description : Text }
                                    | HabiticaTask :
                                        { task :
                                            < HabiticaDaily | HabiticaTodo >
                                        }
                                    | Mail : { id : Text }
                                    >
                                }
                            >
                      }
                  | ChildrenList : { uuid : Text }
                  | DependenciesList : { uuid : Text }
                  | HabiticaList : { list : < HabiticaDailys | HabiticaTodos > }
                  | Mails
                  >
              }
        , uiFeatures :
            { sortInTag : Bool
            , treeOption : < NoTree | PartOfTree | DependsTree >
            }
        }
    }
, filterTag : Text
},
DefinitionElement = < ConfigList : { name : Text, limit : Optional Natural }
| ListElement :
    { item :
        < TaskwarriorTask : { uuid : Text }
        | AdHocTask : { description : Text }
        | HabiticaTask : { task : < HabiticaDaily | HabiticaTodo > }
        | Mail : { id : Text }
        >
    }
>,
ListQuery = < QueryList :
    { query :
        List
          < HasProperty :
              { property :
                  < DescriptionMatches : { filter : Text }
                  | ParentBlocked
                  | Blocked
                  | Waiting
                  | Pending
                  | Completed
                  | Deleted
                  | IsParent
                  | OnList
                  | HasTag : { tag : Text }
                  | HasParent
                  >
              }
          | HasntProperty :
              { property :
                  < DescriptionMatches : { filter : Text }
                  | ParentBlocked
                  | Blocked
                  | Waiting
                  | Pending
                  | Completed
                  | Deleted
                  | IsParent
                  | OnList
                  | HasTag : { tag : Text }
                  | HasParent
                  >
              }
          >
    }
| TagList : { name : Text }
| DefinitionList :
    { elements :
        List
          < ConfigList : { name : Text, limit : Optional Natural }
          | ListElement :
              { item :
                  < TaskwarriorTask : { uuid : Text }
                  | AdHocTask : { description : Text }
                  | HabiticaTask : { task : < HabiticaDaily | HabiticaTodo > }
                  | Mail : { id : Text }
                  >
              }
          >
    }
| ChildrenList : { uuid : Text }
| DependenciesList : { uuid : Text }
| HabiticaList : { list : < HabiticaDailys | HabiticaTodos > }
| Mails
>,
RemoteBackend = { url : Text
, user : Text
, password :
    < Prompt
    | Password : { plaintext : Text }
    | PasswordCommand : { command : Text }
    >
},
ListItem = < TaskwarriorTask : { uuid : Text }
| AdHocTask : { description : Text }
| HabiticaTask : { task : < HabiticaDaily | HabiticaTodo > }
| Mail : { id : Text }
>
}
