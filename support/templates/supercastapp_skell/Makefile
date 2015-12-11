.PHONY: compile  run clean doc

compile:
	@rebar prepare-deps

clean:
	@rebar -r clean

doc:
	@rebar -r doc

run: compile
	./start-dev.sh
