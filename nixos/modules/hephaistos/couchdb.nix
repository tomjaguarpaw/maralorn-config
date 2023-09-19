{
  services.couchdb = {
    enable = true;
    adminUser = "admin";
    adminPass = "admin";
  };
  environment.persistence.snapshoted.directories = [ "/var/lib/couchdb" ];
}
