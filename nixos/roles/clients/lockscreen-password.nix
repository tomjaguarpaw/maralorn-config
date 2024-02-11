{ config, ... }:
{
  users.users.maralorn.hashedPasswordFile = config.age.secrets.pam-short-password.path;
}
