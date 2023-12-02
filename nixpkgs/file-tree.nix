final: _:
let
  inherit (final) lib;
  linkToPath =
    path: fileOrDir:
    (
      if final.lib.types.path.check fileOrDir then
        [
          {
            name = lib.concatStringsSep "/" path;
            path = fileOrDir;
          }
        ]
      else
        lib.concatLists (lib.mapAttrsToList (dirName: linkToPath (path ++ [dirName])) fileOrDir)
    );
in
{
  recursiveLinkFarm = name: files: final.linkFarm name (linkToPath [] files);
}
