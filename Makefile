PREFIX := ../
DEST   := $(PREFIX)$(PROJECT)

.PHONY: all run clean clean-all doc app

all:
	@rebar get-deps
	@rebar -r compile

run: all
	@erl -pa ebin -pa deps/*/ebin -config sys -eval "supercast:start()."

clean:
	@rebar clean

clean-all: clean
	@rm -rf deps
	@rm -rf ebin
	@rm -rf doc

doc:
	@rebar -r doc

app:
	@rebar -r create template=supercastapp dest=$(DEST) appid=$(PROJECT)
