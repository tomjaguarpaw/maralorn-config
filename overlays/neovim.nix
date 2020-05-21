self: super: {
  neovim = super.neovim.override {
    vimAlias = true;
    withPython3 = true;
    withPython = false;
  };
}
