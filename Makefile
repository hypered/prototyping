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
  _site/tables/index.html \
  _site/static/css/style.css


_site/index.html: pages/index.md prototype.hs
	mkdir -p $(dir $@)
	runghc prototype.hs begin-html > $@.temp
	echo "</pre></code>" >> $@.temp
	pandoc $< >> $@.temp
	echo "<code><pre>" >> $@.temp
	runghc prototype.hs end-html >> $@.temp
	mv $@.temp $@

# TODO Add the first characters of the description.
_site/tables/index.html: prototype.db prototype.hs
	mkdir -p $(dir $@)
	runghc prototype.hs table-index-html > $@.temp
	runghc prototype.hs end-html >> $@.temp
	mv $@.temp $@

_site/tables/%.html: tables/%.md prototype.db prototype.hs
	mkdir -p $(dir $@)
	runghc prototype.hs table-html $* > $@.temp
	echo >> $@.temp
	echo "</pre></code>" >> $@.temp
	pandoc $< >> $@.temp
	echo "<code><pre>" >> $@.temp
	echo >> $@.temp
	sqlite3 prototype.db ".schema $*" >> $@.temp
	echo -n "  Defined: " >> $@.temp
	grep -nH "^CREATE TABLE $* ($$" prototype.sql | cut -d : -f 1,2 >> $@.temp
	echo >> $@.temp
	sqlite3 -init sqliterc.txt prototype.db "SELECT * FROM $* LIMIT 100"\
          | grep -v '\-- Loading resources from sqliterc.txt' >> $@.temp
	echo "  Command: sqlite3 prototype.db 'SELECT * FROM $* LIMIT 100'" >> $@.temp
	runghc prototype.hs end-html >> $@.temp
	mv $@.temp $@

_site/static/%.css: static/%.css
	mkdir -p $(dir $@)
	cp $< $@

.PHONY: ghcid
ghcid:
	ghcid "--command=ghci prototype.hs"

.PHONY: clean
clean:
	rm -r _site
