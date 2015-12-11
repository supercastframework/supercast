.PHONY: compile  run clean doc

compile:
	@rebar prepare-deps
	@if [ ! -d priv ]; then cp -r supercast/priv . ; fi

clean:
	@rebar -r clean

doc:
	@rebar -r doc

run: compile
	./start-dev.sh


