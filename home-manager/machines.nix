mylib:
let
  makeConfig = hostName: roles: {
    imports = mylib.nixFromDirs (
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
  athene = makeConfig "athene" [
    "impermanent"
    "metal"
  ];
  hephaistos = makeConfig "hephaistos" [
    "clients"
    "impermanent"
    "laptops"
    "metal"
  ];
  hera = makeConfig "hera" [ ];
  zeus = makeConfig "zeus" [
    "clients"
    "impermanent"
    "metal"
  ];
}
