PREFIX := ../
DEST   := $(PREFIX)$(PROJECT)

REBAR = rebar

.PHONY: compile run clean clean-deps edoc app dialyzer

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
	@$(REBAR) -r clean
	@find doc/ ! -name overview.edoc -type f -delete

clean-deps: clean
	@$(REBAR) delete-deps

app: clean-deps
	@[ -z "$(PROJECT)" ] && echo "ERROR: required variable PROJECT missing" 1>&2 && exit 1 || true
	@$(REBAR) -r create template=simplechannel dest=$(DEST) appid=$(PROJECT)

eqc-compile: clean compile
	rm ebin/*.beam
	@(cd src; erl -pa ../deps/*/ebin ../ebin -noshell -eval \
	"make:files([supercast_proc,supercast_auth,supercast_acctrl,supercast_encoder], [{parse_transform, eqc_cover},{i, \"../include\"}, {outdir, \"../ebin\"}, {d, eqc}])" \
	-s init stop)
	@(cd src; erl -pa ../deps/*/ebin ../ebin -noshell -eval \
	"make:all([{parse_transform, eqc_cover},{i, \"../include\"}, {outdir, \"../ebin\"}, {d, eqc}])" \
	-s init stop)

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
PLT = $(HOME)/.supercast_dialyzer_plt

$(PLT): compile
	dialyzer --build_plt --output_plt $(PLT) --apps $(APPS) deps/*/ebin ebin
	dialyzer --check_plt --plt $(PLT) --apps $(APPS) deps/*/ebin ebin
dialyzer: $(PLT)
	dialyzer -Wno_return --plt $(PLT) deps/*/ebin ebin

