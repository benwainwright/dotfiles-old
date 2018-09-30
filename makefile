all:
	@echo "Generating symlinks..."
	@$(MAKE) -f init/makefile.conf
	@$(MAKE) -f init/makefile.zprezto
