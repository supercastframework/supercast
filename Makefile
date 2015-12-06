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

app:
	@[ -z "$(PROJECT)" ] && echo "ERROR: required variable PROJECT missing" 1>&2 && exit 1 || true
	@$(REBAR) -r create template=supercastapp dest=$(DEST) appid=$(PROJECT)
