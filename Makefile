PREFIX := ../
DEST   := $(PREFIX)$(PROJECT)

REBAR = rebar

.PHONY: all run clean clean-all doc app

all:
	@$(REBAR) -D debug prepare-deps

edoc: all
	@$(REBAR) doc

run: all
	@erl -pa ebin -pa deps/*/ebin -config sys -eval "supercast:start()."

test:
	@rm -rf .eunit
	@mkdir -p .eunit
	@$(REBAR) eunit

clean:
	@$(REBAR) clean

clean-deps: clean
	@$(REBAR) delete-deps

app:
	@[ -z "$(PROJECT)" ] && echo "ERROR: required variable PROJECT missing" 1>&2 && exit 1 || true
	@$(REBAR) -r create template=supercastapp dest=$(DEST) appid=$(PROJECT)
