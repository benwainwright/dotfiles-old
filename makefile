all:
	@echo "Generating symlinks for general dotfiles..."
	@$(MAKE) -f init/makefile.conf
	@echo "Generating symlinks for zprezto dotfiles..."
	@$(MAKE) -f init/makefile.zprezto
