# This Makefile builds prototype and the database, which are necessary for the
# main Makefile.

SOURCES=$(shell find . -name '*.md')
METADATA := $(addprefix _intermediate/metadata/, $(patsubst %.md,%.json,$(SOURCES)))

prototype.db: sql/prototype.sql \
  bin/prototype _intermediate/metadata.sql sql/insert-screens.sql
	rm -f prototype.db
	sqlite3 $@ < $<
	bin/prototype import-md-sources
	sqlite3 $@ < _intermediate/metadata.sql
	sqlite3 prototype.db < sql/insert-screens.sql

# Combine the YAML metadata blocks extracted in the rule below to generate a
# .sql file to insert them into the prototype_metadata table in the above
# `prototype.db` rule.
# The sed replaces the last comma with a semi-colon.
_intermediate/metadata.sql: $(METADATA)
	mkdir -p $(dir $@)
	echo "INSERT INTO prototype_metadata (source, type, data) VALUES" > $@.temp
	for f in $^; do cat $$f | jq -r '"  (\"\(.source)\", \"\(.type)\", \"\(tojson|gsub("\"";"\"\""))\"),"' >> $@.temp.sql ; done
	cat $@.temp.sql | sed '$$ s/,$$/;/' >> $@.temp
	rm $@.temp.sql
	mv $@.temp $@

# Extract YAML metadata block from Markdown files and generate corresponding
# .json files. Those are combined into a .sql file in the
# `_intermediate/metadata.sql` rule above.
_intermediate/metadata/%.json: %.md metadata.tpl
	mkdir -p $(dir $@)
	pandoc \
	  --template metadata.tpl \
	  --to html \
	  --metadata source=$< \
	  --metadata destination=$(patsubst ./%,%,$*).html \
	  --metadata type=markdown \
	  $< -o $@

bin/prototype: bin/prototype.hs
	ghc --make bin/prototype.hs
