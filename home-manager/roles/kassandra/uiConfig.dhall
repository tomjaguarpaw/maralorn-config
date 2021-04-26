let types = ./types.dhall

let Prelude =
      { List.map
        =
          https://prelude.dhall-lang.org/v20.1.0/List/map.dhall sha256:dd845ffb4568d40327f2a817eb42d1c6138b929ca758d50bc33112ef3c885680
      }

let confWid =
      \(name : Text) ->
        types.Widget.ConfigListWidget { name, limit = None Natural }

let listElement =
      \(el : types.ListItem) ->
        types.DefinitionElement.ListElement { item = el }

let simpleTask =
      \(desc : Text) ->
        listElement (types.ListItem.AdHocTask { description = desc })

let defList =
      \(name : Text) ->
      \(list : List types.DefinitionElement) ->
        { name, list = types.ListQuery.DefinitionList { elements = list } }

let defSimpleList =
      \(name : Text) ->
      \(list : List Text) ->
        defList
          name
          (Prelude.List.map Text types.DefinitionElement simpleTask list)

in  { configuredLists =
      [ { name = "Task Baum", list = types.ListQuery.TagList { name = "root" } }
      , defSimpleList
          "Orga Routine"
          [ "Maintenance"
          , "Kassandra 1 Dialog"
          , "Inbox sortieren"
          , "Tasks in Taskbaum einsortieren"
          , "Tagesagenda für morgen mit Taglisten"
          , "Habitica kontrollieren"
          , "Await checken"
          ]
      , defSimpleList "Wochen Routine" [ "Projekte durchgehen" ]
      , defSimpleList
          "Tee kochen"
          [ "Wasser filtern"
          , "Wasser kochen"
          , "Wasser aufgießen"
          , "Teebeutel entnehmen"
          , "Neues Wasser filtern"
          ]
      , defList
          "Morgen Routine"
          [ simpleTask "Wiegen"
          , simpleTask "Podcast anmachen"
          , simpleTask "Anziehen"
          , types.DefinitionElement.ConfigList
              { name = "Tee kochen", limit = None Natural }
          , simpleTask "Zähne Putzen"
          , simpleTask "Rasieren"
          , simpleTask "Frühstücken"
          ]
      ]
    , uiFeatures = { sortInTag = False, treeOption = types.TreeOption.NoTree }
    , viewList =
      [ confWid "Task Baum", confWid "Orga Routine", confWid "Morgen Routine" ]
    }
