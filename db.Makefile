# This Makefile only builds the database, which is necessary for the main
# Makefile.

prototype.db: prototype.sql prototype.hs
	rm -f prototype.db
	sqlite3 $@ < $<
	runghc prototype.hs prototype.db import-md-sources
