{
  programs.ssh = {
    controlMaster = "auto";
    controlPersist = "10m";
    enable = true;
    matchBlocks = {
      git-auto.identityFile = "~/.ssh/id_auto_ed25519";
      "door.cccda.de".identityFile = "~/.ssh/door_rsa";
    };
  };
}
