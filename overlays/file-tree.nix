final: prev:
let
  inherit (prev) lib;
  linkToPath = path: fileOrDir:
    (
      if lib.types.path.check fileOrDir then
        [ "ln -sT ${fileOrDir} ${path}" ]
      else
        [ "mkdir -p ${path}" ] ++ lib.concatLists (
          lib.mapAttrsToList
            (dirName: content: linkToPath "${path}/${dirName}" content)
            fileOrDir
        )
    );
in
{
  setToDirectories = files: prev.runCommand "set-to-directories" { }
    (lib.concatStringsSep "\n" (linkToPath "$out" files));
}
