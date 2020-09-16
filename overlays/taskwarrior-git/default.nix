self: super: {
  taskwarrior-git = self.haskellPackages.callCabal2nix "taskwarrior-git" self.sources.taskwarrior-git-backend {};
}
