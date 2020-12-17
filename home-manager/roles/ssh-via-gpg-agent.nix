{ ... }: {
  services = {
    gpg-agent = {
      defaultCacheTtlSsh = 31536000; # 1year
      enableSshSupport = 31536000; # 1year
    };
  };
}
