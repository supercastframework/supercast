#!/bin/sh
exec erl \
    -pa ebin deps/*/ebin \
    -boot start_sasl \
    -sname {{name}}_dev \
    -config ./sys \
    -eval "{{name}}:start()."
