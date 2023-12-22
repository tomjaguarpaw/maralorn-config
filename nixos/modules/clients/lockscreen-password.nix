{ config, ... }: { users.users.maralorn.passwordFile = config.age.secrets.pam-short-password.path; }
