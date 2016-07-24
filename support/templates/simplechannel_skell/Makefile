.PHONY: compile  run clean doc
REBAR = ./rebar3

compile:
	@ $(REBAR) compile
	@ if [ ! -d priv ]; then cp -r _build/default/lib/supercast/priv . ; fi

clean:
	@ $(REBAR) clean


run: compile
	exec erl \
	    -pa _build/default/lib/*/ebin \
	    -boot start_sasl \
	    -sname {{name}} \
	    -config ./sys \
	    -eval "{{name}}:start()."

