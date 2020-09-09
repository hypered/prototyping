# This is the main Makefile.

DATE=$(shell date --iso-8601=minutes)

# This depends on prototype.hs and prototype.db. The later can be built with
# the db.Makefile. It is simpler to use `scripts/build.sh` to combine both
# steps.
TABLES=$(shell runghc prototype.hs tables)
TABLE_TARGETS := $(addprefix _site/tables/, $(addsuffix .html, $(TABLES)))


.PHONY: all
all: $(TABLE_TARGETS) \
  _site/index.html \
  _site/tables/index.html


_site/index.html: pages/index.md prototype.hs
	mkdir -p $(dir $@)
	runghc prototype.hs begin-html > $@.temp
	cat $< >> $@.temp
	mv $@.temp $@

# TODO Add the first characters of the description.
_site/tables/index.html: prototype.db prototype.hs
	mkdir -p $(dir $@)
	runghc prototype.hs generate-html-index > $@

# TODO Link to the CREATE TABLE statement in prototype.sql, with e.g.
#   grep -nH 'CREATE TABLE directories' prototype.sql | cut -d : -f 2
_site/tables/%.html: tables/%.md prototype.db prototype.hs
	mkdir -p $(dir $@)
	runghc prototype.hs generate-html $* > $@.temp
	echo >> $@.temp
	cat $< >> $@.temp
	echo >> $@.temp
	sqlite3 prototype.db ".schema $*" >> $@.temp
	echo >> $@.temp
	echo "sqlite3 prototype.db 'SELECT * FROM $* LIMIT 100'" >> $@.temp
	sqlite3 prototype.db "SELECT * FROM $* LIMIT 100" >> $@.temp
	mv $@.temp $@

.PHONY: ghcid
ghcid:
	ghcid "--command=ghci prototype.hs"

.PHONY: clean
clean:
	rm -r _site
