lib: rec {
  case = default: attrs: ''
    {% if ${lib.concatStringsSep "\n{% elseif " (lib.mapAttrsToList (condition: result: "${condition} %}\n  ${result}") attrs)}
    {% else %}
      ${default}
    {% endif %}
  '';
  if' = condition: ifTrue: ifFalse: case ifFalse {${condition} = ifTrue;};
  or = lhs: rhs: "(${lhs} or ${rhs})";
  and = lhs: rhs: "(${lhs} and ${rhs})";
  isState = entity: state: "is_state('${entity}','${state}')";
}
