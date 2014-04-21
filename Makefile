# Makefile

ERL         ?= erl
ERLC        ?= erlc -Werror
ASNC        ?= erlc -Werror -bber

DIALYZER    ?= dialyzer
ERLC_DIAL   ?= erlc -Werror +debug_info
ASNC_DIAL   ?= erlc -Werror +debug_info -bber

EPATH       += -pa ebin

APP         = $(subst ebin/,,$(basename $(wildcard ebin/*.app)))
INCLUDE     = $(wildcard ../include/*.hrl)

MAIN        = $(wildcard src/*.erl)
MAIN_DST    = $(patsubst src/%.erl, ebin/%.beam, $(MAIN))

BEHAVIOURS     = $(wildcard src/behaviours/*.erl)
BEHAVIOURS_DST =  $(patsubst src/behaviours/%.erl, ebin/%.beam, $(BEHAVIOURS))

AUTHS       = $(wildcard src/auths/*.erl)
AUTHS_DST   = $(patsubst src/auths/%.erl, ebin/%.beam, $(AUTHS))

ENCODERS    = $(wildcard src/encoders/*.erl)
ENCODERS_DST = $(patsubst src/encoders/%.erl, ebin/%.beam, $(ENCODERS))

ACCTRL      = $(wildcard src/acctrls/*.erl)
ACCTRL_DST  = $(patsubst src/acctrls/%.erl, ebin/%.beam, $(ACCTRL))

compile: $(BEHAVIOURS_DST) $(MAIN_DST) $(AUTHS_DST) \
            $(ENCODERS_DST) $(ACCTRL_DST) pdu


ebin/%.beam: src/%.erl $(INCLUDE)
	$(ERLC) $(EPATH) -o ebin/ src/$*.erl

ebin/%.beam: src/behaviours/%.erl
	$(ERLC) $(EPATH) -o ebin/ src/behaviours/$*.erl

ebin/%.beam: src/auths/%.erl
	$(ERLC) $(EPATH) -o ebin/ src/auths/$*.erl

ebin/%.beam: src/encoders/%.erl
	$(ERLC) $(EPATH) -o ebin/ src/encoders/$*.erl

ebin/%.beam: src/acctrls/%.erl
	$(ERLC) $(EPATH) -o ebin/ src/acctrls/$*.erl

clean:
	rm -f supercast.plt
	rm -f ebin/*.beam
	rm -f *.dump
	rm -f doc/*.html
	rm -f doc/*.png
	rm -f doc/*.css
	rm -f doc/edoc-info
	rm -f priv/asn1/build/*
	rm -f include/ModSupercast.hrl
	rm -f include/ModMonitor.hrl
	rm -f include/ModLocator.hrl

clsupercast:
	@$(ERL) -pa ebin -eval \
 'clsupercast:s(), timer:sleep(500), clsupercast:adm()'

clsupercast2:
	@$(ERL) -pa ebin -eval \
 'clsupercast:s(), timer:sleep(500), clsupercast:std()'
# ASN PDUs
ASN_DEFS    = $(shell find priv/asn1 -name "*.asn")
ASN_DEST    = $(patsubst priv/asn1/%.asn, ebin/%.beam, $(ASN_DEFS))

pdu: $(ASN_DEST)

$(ASN_DEST): ebin/%.beam: priv/asn1/%.asn
	$(ASNC) -o priv/asn1/build priv/asn1/$*.asn
	@cp priv/asn1/build/$*.beam ebin
	@if `test -e priv/asn1/build/$*.hrl`; then cp priv/asn1/build/$*.hrl include; fi


# TEST
test: compile
	@$(ERL) -noinput -pa ebin -eval \
        'eunit:test(bsupercast_acctrl_rbac, [verbose]), init:stop()'

dialize: clean supercast.plt
	$(DIALYZER) --plts supercast.plt -- ebin/

supercast.plt: ERLC = $(ERLC_DIAL)
supercast.plt: ASNC = $(ASNC_DIAL)
supercast.plt: compile
	$(DIALYZER) -Wno_undefined_callbacks --build_plt --output_plt supercast.plt -pa ebin --apps erts kernel stdlib crypto public_key ssl ebin/

# DOCUMENTATION
doc: doc/index.html

doc/index.html: doc/overview.edoc $(MAIN) $(ENCODERS) $(AUTHS) $(ACCTRL) $(BEHAVIOURS)
	@echo "Building documentation..."
	@$(ERL) -noinput -pa ebin -eval \
 'edoc:application($(APP), ".", [{subpackages, true}, {packages, false}]), init:stop()'
