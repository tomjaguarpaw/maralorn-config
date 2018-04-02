default: all

all: links config

links:
	ln -sfT data/aktuell/it/config ~/config
	ln -sfT data/aktuell/it/config/dotfiles ~/.dotfiles
	ln -sfT data/aktuell/it/config/bin ~/bin
	ln -sfT ~/data/aktuell/it/config/nixos ~/.config/nixpkgs/config
	ln -sfT data/aktuell/it/secrets/password-store ~/.password-store
	ln -sfT data/aktuell/it/secrets/gnupg ~/.gnupg

system-links:
	sudo ln -sfT ${HOME}/data/aktuell/it/config/nixos /etc/nixos/config

config:
	rcup rcrc
	rcup
	mkdir -p ~/.vimhist/bak
	touch .chpwd-recent-dirs
