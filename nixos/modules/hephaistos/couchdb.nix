{
  services.couchdb = {
    enable = true;
    adminUser = "admin";
    adminPass = "admin";
    extraConfig = ''
      [chttpd_auth]
      timeout = 7200
    '';
  };
  environment.persistence.snapshoted.directories = ["/var/lib/couchdb"];
}
