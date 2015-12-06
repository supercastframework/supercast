PREFIX := ../
DEST   := $(PREFIX)$(PROJECT)

REBAR = rebar

.PHONY: all run clean clean-all doc app

compile:
	@$(REBAR) -D debug prepare-deps

edoc:
	@$(REBAR) doc

run: compile
	@erl -pa ebin -pa deps/*/ebin -config sys -eval "supercast:start()."

test:
	@rm -rf .eunit
	@mkdir -p .eunit
	@$(REBAR) eunit

clean:
	@$(REBAR) clean
	@rm -rf doc
	@rm -rf ebin

clean-deps: clean
	@$(REBAR) delete-deps
	@rm -rf deps

app:
	@[ -z "$(PROJECT)" ] && echo "ERROR: required variable PROJECT missing" 1>&2 && exit 1 || true
	@$(REBAR) -r create template=supercastapp dest=$(DEST) appid=$(PROJECT)

update-license:
	@echo "--> Updating source headers licenses"
	@for i in $$(/bin/ls src/*.erl include/*.hrl); do \
		echo $$i; \
		awk ' \
			BEGIN {position = "header";} \
			{ \
				if (position == "body") { \
					print; \
				} else { \
					if (length == 0) position = "body"; \
				} \
			} \
		' $$i > temp.awk; \
		cat NOTICE.txt temp.awk > temp.txt; \
		mv temp.txt $$i; \
	done
	@ rm -f temp.awk temp.txt
