prelude:
with prelude;
let
  makeConfig = hostName: roles: {
    imports = nixFromDirs (
      map (x: ./roles + "/${x}") (
        roles
        ++ [
          "all"
          hostName
        ]
      )
    );
    m-0.hostName = hostName;
  };
in
{
  apollo = makeConfig "apollo" [
    "clients"
    "metal"
    "laptops"
  ];
  athene = makeConfig "athene" [
    "impermanent"
    "metal"
    "servers"
  ];
  hephaistos = makeConfig "hephaistos" [
    "clients"
    "impermanent"
    "laptops"
    "metal"
  ];
  hera = makeConfig "hera" [ "servers" ];
  zeus = makeConfig "zeus" [
    "clients"
    "impermanent"
    "metal"
  ];
}
