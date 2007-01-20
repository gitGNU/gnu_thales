data_DATA = mysql-bahamut.sql mysql-unreal.sql mysql-ultimate28.sql mysql-ultimate30.sql mysql-hybrid.sql mysql-ircdru.sql

sysconf_DATA = thales.conf.default

EXTRA_DIST = mysql-bahamut.sql mysql-unreal.sql mysql-ultimate28.sql mysql-ultimate30.sql mysql-hybrid.sql mysql-ircdru.sql thales.conf.default

install-data-hook:
	@if test -f $(DESTDIR)$(sysconfdir)/thales.conf ; then \
		echo "warning: not overwriting existing thales.conf" >&2 ; \
	else \
		cp $(DESTDIR)$(sysconfdir)/thales.conf.default \
		   $(DESTDIR)$(sysconfdir)/thales.conf ; \
	fi

uninstall-hook:
	rm -f $(DESTDIR)$(sysconfdir)/thales.conf
