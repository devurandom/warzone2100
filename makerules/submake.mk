.PHONY: all clean $(SUBDIRS)

all clean: $(SUBDIRS)

$(SUBDIRS):
	-$(MKDIR_P) $(builddir)/$@
	$(MAKE) -f $(srcdir)/$@/makefile.raw -C $(builddir)/$@ $(MAKECMDGOALS)

clean: $(SUBDIRS)
	$(RMDIR) $(SUBDIRS)
