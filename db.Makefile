# This Makefile only builds the database, which is necessary for the main
# Makefile.

SOURCES=$(shell find . -name '*.md')
METADATA := $(addprefix _intermediate/metadata/, $(patsubst %.md,%.json,$(SOURCES)))

prototype.db: prototype.sql prototype.hs _intermediate/metadata.sql
	rm -f prototype.db
	sqlite3 $@ < $<
	runghc prototype.hs import-md-sources
	sqlite3 $@ < _intermediate/metadata.sql

# The sed replaces the last comma with a semi-colon.
_intermediate/metadata.sql: $(METADATA)
	mkdir -p $(dir $@)
	echo "INSERT INTO prototype_metadata (source, type, data) VALUES" > $@.temp
	for f in $^; do cat $$f | jq -r '"  (\"\(.source)\", \"\(.type)\", \"\(tojson|gsub("\"";"\"\""))\"),"' >> $@.temp.sql ; done
	cat $@.temp.sql | sed '$$ s/,$$/;/' >> $@.temp
	rm $@.temp.sql
	mv $@.temp $@

_intermediate/metadata/%.json: %.md metadata.tpl
	mkdir -p $(dir $@)
	pandoc \
	  --template metadata.tpl \
	  --to html \
	  --metadata source=$< \
	  --metadata type=markdown \
	  $< -o $@
