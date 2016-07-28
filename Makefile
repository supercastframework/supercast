.PHONY: compile run clean clean-deps edoc app dialyzer

REBAR = rebar3

compile:
	@$(REBAR) compile

edoc:
	@$(REBAR) edoc
	@printf "\nbody {font-family: \"Helvetica Neue\",Helvetica,Roboto,Arial,sans-serif;}\n" \
		>> doc/stylesheet.css

clean:
	@$(REBAR) clean
	@find doc/ ! -name overview.edoc -type f -delete

dialyzer:
	$(REBAR) dialyzer

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
