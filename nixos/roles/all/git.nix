{
  programs.git.config = {
    init.defaultBranch = "main";
    url = {
      "ssh://git@github.com/".insteadOf = [ "gh:" ];
      "ssh://gitea@code.maralorn.de/".insteadOf = [ "code:" ];
      "ssh://git@gitlab.heilmann-software.net/".insteadOf = [ "hs:" ];
    };
  };
}
