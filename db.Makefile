# This Makefile only builds the database, which is necessary for the main
# Makefile.

SOURCES=$(shell find . -name '*.md')
METADATA := $(addprefix _intermediate/metadata/, $(patsubst %.md,%.json,$(SOURCES)))

prototype.db: prototype.sql prototype.hs _intermediate/metadata.sql
	rm -f prototype.db
	sqlite3 $@ < $<
	runghc prototype.hs import-md-sources
	sqlite3 $@ < _intermediate/metadata.sql

# Combine the YAML metadata blocks extrqcted in the rule below to generate a
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
	  --metadata type=markdown \
	  $< -o $@
