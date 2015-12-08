PREFIX := ../
DEST   := $(PREFIX)$(PROJECT)

REBAR = rebar

.PHONY: all run clean clean-deps edoc app

compile:
	@$(REBAR) -D debug prepare-deps

edoc:
	@$(REBAR) doc
	@printf "\nbody {font-family: \"Helvetica Neue\",Helvetica,Roboto,Arial,sans-serif;}\n" \
		>> doc/stylesheet.css

run: compile
	@erl -pa ebin -pa deps/*/ebin -config sys -eval "supercast:start()."

test: compile
	@rm -rf .eunit
	@mkdir -p .eunit
	@$(REBAR) skip_deps=true eunit

clean:
	@$(REBAR) clean
	@rm -rf doc

clean-deps: clean
	@$(REBAR) delete-deps

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

# dialyzer
APPS = kernel stdlib sasl erts ssl tools os_mon runtime_tools crypto inets \
	xmerl webtool snmp public_key mnesia eunit syntax_tools compiler
DEPS = deps/cowboy/ebin deps/cowlib/ebin deps/ranch/ebin deps/jsx/ebin

PLT = $(HOME)/.supercast_dialyzer_plt
$(PLT): compile
	dialyzer --build_plt --output_plt $(PLT) --apps $(APPS) $(DEPS) ebin
	dialyzer --check_plt --plt $(PLT) --apps $(APPS) $(DEPS) ebin
dialyzer: $(PLT)
	dialyzer -Wno_return --plt $(PLT) ebin

