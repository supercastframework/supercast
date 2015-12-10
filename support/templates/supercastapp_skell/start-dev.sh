#!/bin/sh
exec erl \
    -pa ebin deps/*/ebin \
    -boot start_sasl \
    -sname {{appid}}_dev \
    -eval "{{appid}}:start()."
