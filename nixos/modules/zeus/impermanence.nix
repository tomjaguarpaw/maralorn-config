{
  environment.persistence."/disk/persist" = {
    users.maralorn.directories = [
      ".cache/rbw" # Save user login
      "Games"
    ];
  };
  environment.persistence."/disk/volatile" = {
    users.maralorn.directories = [
      ".factorio" # Factorio save games and login
      ".config/heroic" # Login data
    ];
  };
}
