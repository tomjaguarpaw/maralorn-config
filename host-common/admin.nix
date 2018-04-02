let
  keys = [
  "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQC+jbK/gzzarMHQc9R++i+llMHIh34lXr7FjIyjCdVjnLjKDE3mdJ6mh7JTJc9njn7s+6kZ7FAoDWe9QValR0OUlE3TRAD3wtu0Kud7LUPsR961Go84yRw3mVMZpJFJHYb4p2bTPcMMgFnj8+b5RfWJ1GU5gMOT7EIkpmytpien/IvBig8dzNQ152YQU9xiQ9dZspsMiSMP0pt5mU5tqGGA/5WCXgUPk19OhhEkak/VMNFPnRysk5ofmYWbitShciMAnTx1UHyDYCzbiHHN8Ud9UxkSVoR+q9RYbMfXsW04z5z8sRna1xDo8N9c3bZgcUXIBlIVBJPOaABpXO6+Ke3X pegasus"
  "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQC+r1uf2Wuw3CwXS8HaU5fl99LL74Xnblr0SWoUGKHKoRqgtNg/a+Z++l87UhqBGm9eYuFLSYGRzzQiffGbtfRT/18G/pixiPYPQCOQp7lWRguGjs4ejGzgIy9CrAXEKDwI6294tvX/8WiQLckROYE2gVKyA70yM0QmlozwqU9mzsky81EwDOtltsQGbBTswVuzNHqMgZsDTg+aBd66qUSRWMgh3PfvQyJPd+EUrsQzdt6lTx1A/Vg2oPXP3xZIKpbgQzDXxtzz1a7H/QYkJkkjefAFeMa3Z+PpP4zFV0Wnrn3Ny4sC4kjgywt1CPwJX7WnrZxpQ3cmg3G/08MAl+wp maralorn@apollo"
  "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQCvKq3AkhHDKTPMKhXZfZhLhgwqk186h1R/j2rDNYPqsh73AnfiyTZQQqX4O7chkFCEFsBmDrpaOg1fnzIA0OGYIx02KwUQa8W/1eC4AsgDVQGdRCVKNQ+LFrtFQB0yjzO1zzXMpp3/BhfkwftyrAPP431icum/bMgpfz+QexeSbmmQUXVydcSK02YLJHbDwaF2FxTD1gfoLcCdW36VCp6xBPDVrvlp6z9mU+TZS0UxG8ruh3Q9FFGQqJ6NTy14dj4H5pHW9toFB3uDqBltfoN6azW+DylyxgM6/0PwQn3rALmPv3Ye/Wp/p12o0YhFXWbFRhnrSv1zomU/xjSm0LXz brandy@fb04217" ];
  pw-file = "/etc/nixos/local/pw-maralorn";
in
{
  users.users = {
    maralorn = {
      description = "Malte Brandy";
      isNormalUser = true;
      uid = 1000;
      extraGroups = [ "wheel" "systemd-journal" "networkmanager" "docker" ];
      openssh.authorizedKeys.keys = keys;
      passwordFile = pw-file;
    };
    root = {
      openssh.authorizedKeys.keys = keys;
      passwordFile = pw-file;
    };
  };
}
