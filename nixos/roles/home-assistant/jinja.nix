lib:
let
  inherit (builtins) foldl';
in
rec {
  case = default: attrs: ''
    {% if ${
      lib.concatStringsSep
        ''

          {% elseif ''
        (
          lib.mapAttrsToList
            (condition: result: ''
              ${condition} %}
                ${result}'')
            attrs
        )
    }
    {% else %}
      ${default}
    {% endif %}
  '';
  if' =
    condition: ifTrue: ifFalse:
    case ifFalse { "${condition}" = ifTrue; };
  or = lhs: rhs: "(${lhs} or ${rhs})";
  and = lhs: rhs: "(${lhs} and ${rhs})";
  isState = entity: state: "is_state('${entity}','${state}')";
  isStates = entity: states: foldl' or "false" (map (isState entity) states);
}
