{
  programs.ssh = {
    controlMaster = "auto";
    controlPersist = "10m";
    enable = true;
    matchBlocks = {
      git-auto.identityFile = "~/.ssh/id_auto_ed25519";
      "door.w17.io".identityFile = "~/.ssh/door_rsa";
    };
  };
}
