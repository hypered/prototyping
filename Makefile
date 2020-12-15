# This is the main Makefile.

DATE=$(shell date --iso-8601=minutes)

# This depends on prototype and prototype.db. They can be built with the
# db.Makefile. It is simpler to use `scripts/build.sh` to combine both steps.
TABLES=$(shell bin/prototype tables)
TABLE_TARGETS := $(addprefix _site/tables/, $(addsuffix .html, $(TABLES)))
SCREENS=$(shell bin/prototype screens)
SCREEN_TARGETS := $(addprefix _site/screens/, $(addsuffix .html, $(SCREENS)))


.PHONY: all
all: $(TABLE_TARGETS) \
  $(SCREEN_TARGETS) \
  _site/index.html \
  _site/80-characters.html \
  _site/database.html \
  _site/screens/index.html \
  _site/tables/index.html \
  _site/static/css/style.css

_site/%.html: _intermediate/pages/%.html \
  _intermediate/begin.html _intermediate/end.html
	mkdir -p $(dir $@)
	cat _intermediate/begin.html >> $@.temp
	echo "</code></pre>" >> $@.temp
	cat $< >> $@.temp
	echo "<pre><code>" >> $@.temp
	cat _intermediate/end.html >> $@.temp
	mv $@.temp $@

_site/screens/index.html: prototype.db bin/prototype \
  _intermediate/end.html
	mkdir -p $(dir $@)
	bin/prototype screen-index-html > $@.temp
	cat _intermediate/end.html >> $@.temp
	mv $@.temp $@

# The || true below is necessary because when no record exist in the table,
# the grep invokation will exit 1, instead of 0.
_site/screens/%.html: screens/%.md prototype.db bin/prototype \
  _intermediate/end.html
	mkdir -p $(dir $@)
	bin/prototype screen-html $* > $@.temp
	echo >> $@.temp
	echo "</code></pre>" >> $@.temp
	pandoc $< >> $@.temp
	echo "<pre><code>" >> $@.temp
	cat _intermediate/end.html >> $@.temp
	mv $@.temp $@

_site/tables/index.html: prototype.db bin/prototype \
  _intermediate/end.html
	mkdir -p $(dir $@)
	bin/prototype table-index-html > $@.temp
	cat _intermediate/end.html >> $@.temp
	mv $@.temp $@

# The || true below is necessary because when no record exist in the table,
# the grep invokation will exit 1, instead of 0.
_site/tables/%.html: tables/%.md prototype.db bin/prototype \
  _intermediate/end.html
	mkdir -p $(dir $@)
	bin/prototype table-html $* > $@.temp
	echo >> $@.temp
	echo "</code></pre>" >> $@.temp
	pandoc $< >> $@.temp
	echo "<pre><code>" >> $@.temp
	echo >> $@.temp
	sqlite3 prototype.db ".schema $*" >> $@.temp
	echo -n "  Defined: " >> $@.temp
	grep -nH "^CREATE TABLE $* ($$" sql/prototype.sql | cut -d : -f 1,2 \
	  | sed 's@\([^:]\+\):\(.\+\)@<a href="/database.html#cb1-\2">\1:\2</a>@' \
	  >> $@.temp
	echo >> $@.temp
	sqlite3 -init .sqliterc.txt prototype.db "SELECT * FROM $* LIMIT 100"\
          | grep -v '\-- Loading resources from .sqliterc.txt' >> $@.temp || true
	echo "  Command: sqlite3 prototype.db \"SELECT * FROM $* LIMIT 100\"" >> $@.temp
	cat _intermediate/end.html >> $@.temp
	mv $@.temp $@

_site/static/%.css: static/%.css
	mkdir -p $(dir $@)
	cp $< $@

_intermediate/begin.html: bin/prototype
	mkdir -p $(dir $@)
	bin/prototype begin-html > $@

_intermediate/end.html: bin/prototype
	mkdir -p $(dir $@)
	bin/prototype end-html > $@

# Same rule as below but depends on prototype.sql.
_intermediate/pages/database.html: pages/database.md filters/include-filter.hs \
  sql/prototype.sql
	mkdir -p $(dir $@)
	cat code-style.html > $@
	pandoc \
	  --highlight=kate \
	  --filter filters/include-filter.hs \
	  $< >> $@

_intermediate/pages/%.html: pages/%.md filters/include-filter.hs
	mkdir -p $(dir $@)
	pandoc \
	  --highlight=kate \
	  --filter filters/include-filter.hs \
	  $< >> $@

.PHONY: ghcid
ghcid:
	ghcid "--command=ghci prototype.hs"

.PHONY: clean
clean:
	rm -rf prototype.db _intermediate _site \
	  prototype prototype.hi prototype.o Prototype.hi Prototype.o
