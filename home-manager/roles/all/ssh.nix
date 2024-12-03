{
  programs.ssh = {
    controlMaster = "auto";
    controlPersist = "10m";
    enable = true;
    matchBlocks = {
      "console.heilmannsoftware.net".forwardAgent = true;
      git-auto.identityFile = "~/.ssh/id_auto_ed25519";
      "door.cccda.de".identityFile = "~/.ssh/door_rsa";
    };
  };
}
