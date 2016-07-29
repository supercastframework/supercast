.PHONY: compile run clean clean-deps doc app dialyzer

REBAR = ./rebar3

compile:
	@$(REBAR) compile

clean:
	@$(REBAR) clean
	@find doc/ ! -name overview.edoc -type f -delete

clean_all: clean
	rm -rf _build

test:
	$(REBAR) eunit

dialyzer:
	$(REBAR) dialyzer

doc:
	@$(REBAR) edoc
	@printf "\nbody {font-family: \"Helvetica Neue\",Helvetica,Roboto,Arial,sans-serif;}\n" \
		>> doc/stylesheet.css

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
