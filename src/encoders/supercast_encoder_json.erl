%% @author Bob Ippolito <bob@mochimedia.com>
%% @copyright 2007 Mochi Media, Inc.
%% @private

%% @doc Yet another JSON (RFC 4627) library for Erlang. mochijson2 works
%%      with binaries as strings, arrays as lists (without an {array, _})
%%      wrapper and it only knows how to decode UTF-8 (and ASCII).
%%
%%      JSON terms are decoded as follows (javascript -> erlang):
%%      <ul>
%%          <li>{"key": "value"} ->
%%              {struct, [{&lt;&lt;"key">>, &lt;&lt;"value">>}]}</li>
%%          <li>["array", 123, 12.34, true, false, null] ->
%%              [&lt;&lt;"array">>, 123, 12.34, true, false, null]
%%          </li>
%%      </ul>
%%      <ul>
%%          <li>Strings in JSON decode to UTF-8 binaries in Erlang</li>
%%          <li>Objects decode to {struct, PropList}</li>
%%          <li>Numbers decode to integer or float</li>
%%          <li>true, false, null decode to their respective terms.</li>
%%      </ul>
%%      The encoder will accept the same format that the decoder will produce,
%%      but will also allow additional cases for leniency:
%%      <ul>
%%          <li>atoms other than true, false, null will be considered UTF-8
%%              strings (even as a proplist key)
%%          </li>
%%          <li>{json, IoList} will insert IoList directly into the output
%%              with no validation
%%          </li>
%%          <li>{array, Array} will be encoded as Array
%%              (legacy mochijson style)
%%          </li>
%%          <li>A non-empty raw proplist will be encoded as an object as long
%%              as the first pair does not have an atom key of json, struct,
%%              or array
%%          </li>
%%      </ul>
%% @end

-module(supercast_encoder_json).
-behaviour(supercast_encoder).
-author('bob@mochimedia.com').
-export([encoder/1, encode_mochi/1]).
-export([decoder/1, decode_mochi/1, decode_mochi/2]).
-export([digits/1, frexp/1, int_pow/2, int_ceil/1]).

%% exports for supercast
-export([encode/1, decode/1]).

%% IEEE 754 Float exponent bias
-define(FLOAT_BIAS, 1022).
-define(MIN_EXP, -1074).
-define(BIG_POW, 4503599627370496).

%% This is a macro to placate syntax highlighters..
-define(Q, $\").
-define(ADV_COL(S, N), S#decoder{offset=N+S#decoder.offset,
                                 column=N+S#decoder.column}).
-define(INC_COL(S), S#decoder{offset=1+S#decoder.offset,
                              column=1+S#decoder.column}).
-define(INC_LINE(S), S#decoder{offset=1+S#decoder.offset,
                               column=1,
                               line=1+S#decoder.line}).
-define(INC_CHAR(S, C),
        case C of
            $\n ->
                S#decoder{column=1,
                          line=1+S#decoder.line,
                          offset=1+S#decoder.offset};
            _ ->
                S#decoder{column=1+S#decoder.column,
                          offset=1+S#decoder.offset}
        end).
-define(IS_WHITESPACE(C),
        (C =:= $\s orelse C =:= $\t orelse C =:= $\r orelse C =:= $\n)).

%% @type json_string() = atom | binary()
%% @type json_number() = integer() | float()
%% @type json_array() = [json_term()]
%% @type json_object() = {struct, [{json_string(), json_term()}]}
%% @type json_eep18_object() = {[{json_string(), json_term()}]}
%% @type json_iolist() = {json, iolist()}
%% @type json_term() = json_string() | json_number() | json_array() |
%%                     json_object() | json_eep18_object() | json_iolist()

-record(encoder, {handler=null,
                  utf8=false}).

-record(decoder, {object_hook=null,
                  offset=0,
                  line=1,
                  column=1,
                  state=null}).

encode(_ETerm) ->
    not_implemented.

decode(_Json) ->
    not_implemented.

%% @spec encoder([encoder_option()]) -> function()
%% @doc Create an encoder/1 with the given options.
%% @type encoder_option() = handler_option() | utf8_option()
%% @type utf8_option() = boolean(). Emit unicode as utf8 (default - false)
encoder(Options) ->
    State = parse_encoder_options(Options, #encoder{}),
    fun (O) -> json_encode(O, State) end.

%% @spec encode_mochi(json_term()) -> iolist()
%% @doc Encode the given as JSON to an iolist.
encode_mochi(Any) ->
    json_encode(Any, #encoder{}).

%% @spec decoder([decoder_option()]) -> function()
%% @doc Create a decoder/1 with the given options.
decoder(Options) ->
    State = parse_decoder_options(Options, #decoder{}),
    fun (O) -> json_decode(O, State) end.

%% @spec decode_mochi(iolist(), [{format, proplist | eep18 | struct}]) -> json_term()
%% @doc Decode the given iolist to Erlang terms using the given object format
%%      for decoding, where proplist returns JSON objects as [{binary(), json_term()}]
%%      proplists, eep18 returns JSON objects as {[binary(), json_term()]}, and struct
%%      returns them as-is.
decode_mochi(S, Options) ->
    json_decode(S, parse_decoder_options(Options, #decoder{})).

%% @spec decode_mochi(iolist()) -> json_term()
%% @doc Decode the given iolist to Erlang terms.
decode_mochi(S) ->
    json_decode(S, #decoder{}).

%% Internal API

parse_encoder_options([], State) ->
    State;
parse_encoder_options([{handler, Handler} | Rest], State) ->
    parse_encoder_options(Rest, State#encoder{handler=Handler});
parse_encoder_options([{utf8, Switch} | Rest], State) ->
    parse_encoder_options(Rest, State#encoder{utf8=Switch}).

parse_decoder_options([], State) ->
    State;
parse_decoder_options([{object_hook, Hook} | Rest], State) ->
    parse_decoder_options(Rest, State#decoder{object_hook=Hook});
parse_decoder_options([{format, Format} | Rest], State)
  when Format =:= struct orelse Format =:= eep18 orelse Format =:= proplist ->
    parse_decoder_options(Rest, State#decoder{object_hook=Format}).

json_encode(true, _State) ->
    <<"true">>;
json_encode(false, _State) ->
    <<"false">>;
json_encode(null, _State) ->
    <<"null">>;
json_encode(I, _State) when is_integer(I) ->
    integer_to_list(I);
json_encode(F, _State) when is_float(F) ->
    digits(F);
json_encode(S, State) when is_binary(S); is_atom(S) ->
    json_encode_string(S, State);
json_encode([{K, _}|_] = Props, State) when (K =/= struct andalso
                                             K =/= array andalso
                                             K =/= json) ->
    json_encode_proplist(Props, State);
json_encode({struct, Props}, State) when is_list(Props) ->
    json_encode_proplist(Props, State);
json_encode({Props}, State) when is_list(Props) ->
    json_encode_proplist(Props, State);
json_encode({}, State) ->
    json_encode_proplist([], State);
json_encode(Array, State) when is_list(Array) ->
    json_encode_array(Array, State);
json_encode({array, Array}, State) when is_list(Array) ->
    json_encode_array(Array, State);
json_encode({json, IoList}, _State) ->
    IoList;
json_encode(Bad, #encoder{handler=null}) ->
    exit({json_encode, {bad_term, Bad}});
json_encode(Bad, State=#encoder{handler=Handler}) ->
    json_encode(Handler(Bad), State).

json_encode_array([], _State) ->
    <<"[]">>;
json_encode_array(L, State) ->
    F = fun (O, Acc) ->
                [$,, json_encode(O, State) | Acc]
        end,
    [$, | Acc1] = lists:foldl(F, "[", L),
    lists:reverse([$\] | Acc1]).

json_encode_proplist([], _State) ->
    <<"{}">>;
json_encode_proplist(Props, State) ->
    F = fun ({K, V}, Acc) ->
                KS = json_encode_string(K, State),
                VS = json_encode(V, State),
                [$,, VS, $:, KS | Acc]
        end,
    [$, | Acc1] = lists:foldl(F, "{", Props),
    lists:reverse([$\} | Acc1]).

json_encode_string(A, State) when is_atom(A) ->
    L = atom_to_list(A),
    case json_string_is_safe(L) of
        true ->
            [?Q, L, ?Q];
        false ->
            json_encode_string_unicode(xmerl_ucs:from_utf8(L), State, [?Q])
    end;
json_encode_string(B, State) when is_binary(B) ->
    case json_bin_is_safe(B) of
        true ->
            [?Q, B, ?Q];
        false ->
            json_encode_string_unicode(xmerl_ucs:from_utf8(B), State, [?Q])
    end;
json_encode_string(I, _State) when is_integer(I) ->
    [?Q, integer_to_list(I), ?Q];
json_encode_string(L, State) when is_list(L) ->
    case json_string_is_safe(L) of
        true ->
            [?Q, L, ?Q];
        false ->
            json_encode_string_unicode(L, State, [?Q])
    end.

json_string_is_safe([]) ->
    true;
json_string_is_safe([C | Rest]) ->
    case C of
        ?Q ->
            false;
        $\\ ->
            false;
        $\b ->
            false;
        $\f ->
            false;
        $\n ->
            false;
        $\r ->
            false;
        $\t ->
            false;
        C when C >= 0, C < $\s; C >= 16#7f, C =< 16#10FFFF ->
            false;
        C when C < 16#7f ->
            json_string_is_safe(Rest);
        _ ->
            false
    end.

json_bin_is_safe(<<>>) ->
    true;
json_bin_is_safe(<<C, Rest/binary>>) ->
    case C of
        ?Q ->
            false;
        $\\ ->
            false;
        $\b ->
            false;
        $\f ->
            false;
        $\n ->
            false;
        $\r ->
            false;
        $\t ->
            false;
        C when C >= 0, C < $\s; C >= 16#7f ->
            false;
        C when C < 16#7f ->
            json_bin_is_safe(Rest)
    end.

json_encode_string_unicode([], _State, Acc) ->
    lists:reverse([$\" | Acc]);
json_encode_string_unicode([C | Cs], State, Acc) ->
    Acc1 = case C of
               ?Q ->
                   [?Q, $\\ | Acc];
               %% Escaping solidus is only useful when trying to protect
               %% against "</script>" injection attacks which are only
               %% possible when JSON is inserted into a HTML document
               %% in-line. mochijson2 does not protect you from this, so
               %% if you do insert directly into HTML then you need to
               %% uncomment the following case or escape the output of encode.
               %%
               %% $/ ->
               %%    [$/, $\\ | Acc];
               %%
               $\\ ->
                   [$\\, $\\ | Acc];
               $\b ->
                   [$b, $\\ | Acc];
               $\f ->
                   [$f, $\\ | Acc];
               $\n ->
                   [$n, $\\ | Acc];
               $\r ->
                   [$r, $\\ | Acc];
               $\t ->
                   [$t, $\\ | Acc];
               C when C >= 0, C < $\s ->
                   [unihex(C) | Acc];
               C when C >= 16#7f, C =< 16#10FFFF, State#encoder.utf8 ->
                   [xmerl_ucs:to_utf8(C) | Acc];
               C when  C >= 16#7f, C =< 16#10FFFF, not State#encoder.utf8 ->
                   [unihex(C) | Acc];
               C when C < 16#7f ->
                   [C | Acc];
               _ ->
                   exit({json_encode, {bad_char, C}})
           end,
    json_encode_string_unicode(Cs, State, Acc1).

hexdigit(C) when C >= 0, C =< 9 ->
    C + $0;
hexdigit(C) when C =< 15 ->
    C + $a - 10.

unihex(C) when C < 16#10000 ->
    <<D3:4, D2:4, D1:4, D0:4>> = <<C:16>>,
    Digits = [hexdigit(D) || D <- [D3, D2, D1, D0]],
    [$\\, $u | Digits];
unihex(C) when C =< 16#10FFFF ->
    N = C - 16#10000,
    S1 = 16#d800 bor ((N bsr 10) band 16#3ff),
    S2 = 16#dc00 bor (N band 16#3ff),
    [unihex(S1), unihex(S2)].

json_decode(L, S) when is_list(L) ->
    json_decode(iolist_to_binary(L), S);
json_decode(B, S) ->
    {Res, S1} = decode1(B, S),
    {eof, _} = tokenize(B, S1#decoder{state=trim}),
    Res.

decode1(B, S=#decoder{state=null}) ->
    case tokenize(B, S#decoder{state=any}) of
        {{const, C}, S1} ->
            {C, S1};
        {start_array, S1} ->
            decode_array(B, S1);
        {start_object, S1} ->
            decode_object(B, S1)
    end.

make_object(V, #decoder{object_hook=N}) when N =:= null orelse N =:= struct ->
    V;
make_object({struct, P}, #decoder{object_hook=eep18}) ->
    {P};
make_object({struct, P}, #decoder{object_hook=proplist}) ->
    P;
make_object(V, #decoder{object_hook=Hook}) ->
    Hook(V).

decode_object(B, S) ->
    decode_object(B, S#decoder{state=key}, []).

decode_object(B, S=#decoder{state=key}, Acc) ->
    case tokenize(B, S) of
        {end_object, S1} ->
            V = make_object({struct, lists:reverse(Acc)}, S1),
            {V, S1#decoder{state=null}};
        {{const, K}, S1} ->
            {colon, S2} = tokenize(B, S1),
            {V, S3} = decode1(B, S2#decoder{state=null}),
            decode_object(B, S3#decoder{state=comma}, [{K, V} | Acc])
    end;
decode_object(B, S=#decoder{state=comma}, Acc) ->
    case tokenize(B, S) of
        {end_object, S1} ->
            V = make_object({struct, lists:reverse(Acc)}, S1),
            {V, S1#decoder{state=null}};
        {comma, S1} ->
            decode_object(B, S1#decoder{state=key}, Acc)
    end.

decode_array(B, S) ->
    decode_array(B, S#decoder{state=any}, []).

decode_array(B, S=#decoder{state=any}, Acc) ->
    case tokenize(B, S) of
        {end_array, S1} ->
            {lists:reverse(Acc), S1#decoder{state=null}};
        {start_array, S1} ->
            {Array, S2} = decode_array(B, S1),
            decode_array(B, S2#decoder{state=comma}, [Array | Acc]);
        {start_object, S1} ->
            {Array, S2} = decode_object(B, S1),
            decode_array(B, S2#decoder{state=comma}, [Array | Acc]);
        {{const, Const}, S1} ->
            decode_array(B, S1#decoder{state=comma}, [Const | Acc])
    end;
decode_array(B, S=#decoder{state=comma}, Acc) ->
    case tokenize(B, S) of
        {end_array, S1} ->
            {lists:reverse(Acc), S1#decoder{state=null}};
        {comma, S1} ->
            decode_array(B, S1#decoder{state=any}, Acc)
    end.

tokenize_string(B, S=#decoder{offset=O}) ->
    case tokenize_string_fast(B, O) of
        {escape, O1} ->
            Length = O1 - O,
            S1 = ?ADV_COL(S, Length),
            <<_:O/binary, Head:Length/binary, _/binary>> = B,
            tokenize_string(B, S1, lists:reverse(binary_to_list(Head)));
        O1 ->
            Length = O1 - O,
            <<_:O/binary, String:Length/binary, ?Q, _/binary>> = B,
            {{const, String}, ?ADV_COL(S, Length + 1)}
    end.

tokenize_string_fast(B, O) ->
    case B of
        <<_:O/binary, ?Q, _/binary>> ->
            O;
        <<_:O/binary, $\\, _/binary>> ->
            {escape, O};
        <<_:O/binary, C1, _/binary>> when C1 < 128 ->
            tokenize_string_fast(B, 1 + O);
        <<_:O/binary, C1, C2, _/binary>> when C1 >= 194, C1 =< 223,
                C2 >= 128, C2 =< 191 ->
            tokenize_string_fast(B, 2 + O);
        <<_:O/binary, C1, C2, C3, _/binary>> when C1 >= 224, C1 =< 239,
                C2 >= 128, C2 =< 191,
                C3 >= 128, C3 =< 191 ->
            tokenize_string_fast(B, 3 + O);
        <<_:O/binary, C1, C2, C3, C4, _/binary>> when C1 >= 240, C1 =< 244,
                C2 >= 128, C2 =< 191,
                C3 >= 128, C3 =< 191,
                C4 >= 128, C4 =< 191 ->
            tokenize_string_fast(B, 4 + O);
        _ ->
            throw(invalid_utf8)
    end.

tokenize_string(B, S=#decoder{offset=O}, Acc) ->
    case B of
        <<_:O/binary, ?Q, _/binary>> ->
            {{const, iolist_to_binary(lists:reverse(Acc))}, ?INC_COL(S)};
        <<_:O/binary, "\\\"", _/binary>> ->
            tokenize_string(B, ?ADV_COL(S, 2), [$\" | Acc]);
        <<_:O/binary, "\\\\", _/binary>> ->
            tokenize_string(B, ?ADV_COL(S, 2), [$\\ | Acc]);
        <<_:O/binary, "\\/", _/binary>> ->
            tokenize_string(B, ?ADV_COL(S, 2), [$/ | Acc]);
        <<_:O/binary, "\\b", _/binary>> ->
            tokenize_string(B, ?ADV_COL(S, 2), [$\b | Acc]);
        <<_:O/binary, "\\f", _/binary>> ->
            tokenize_string(B, ?ADV_COL(S, 2), [$\f | Acc]);
        <<_:O/binary, "\\n", _/binary>> ->
            tokenize_string(B, ?ADV_COL(S, 2), [$\n | Acc]);
        <<_:O/binary, "\\r", _/binary>> ->
            tokenize_string(B, ?ADV_COL(S, 2), [$\r | Acc]);
        <<_:O/binary, "\\t", _/binary>> ->
            tokenize_string(B, ?ADV_COL(S, 2), [$\t | Acc]);
        <<_:O/binary, "\\u", C3, C2, C1, C0, Rest/binary>> ->
            C = erlang:list_to_integer([C3, C2, C1, C0], 16),
            if C > 16#D7FF, C < 16#DC00 ->
                %% coalesce UTF-16 surrogate pair
                <<"\\u", D3, D2, D1, D0, _/binary>> = Rest,
                D = erlang:list_to_integer([D3,D2,D1,D0], 16),
                [CodePoint] = xmerl_ucs:from_utf16be(<<C:16/big-unsigned-integer,
                    D:16/big-unsigned-integer>>),
                Acc1 = lists:reverse(xmerl_ucs:to_utf8(CodePoint), Acc),
                tokenize_string(B, ?ADV_COL(S, 12), Acc1);
            true ->
                Acc1 = lists:reverse(xmerl_ucs:to_utf8(C), Acc),
                tokenize_string(B, ?ADV_COL(S, 6), Acc1)
            end;
        <<_:O/binary, C1, _/binary>> when C1 < 128 ->
            tokenize_string(B, ?INC_CHAR(S, C1), [C1 | Acc]);
        <<_:O/binary, C1, C2, _/binary>> when C1 >= 194, C1 =< 223,
                C2 >= 128, C2 =< 191 ->
            tokenize_string(B, ?ADV_COL(S, 2), [C2, C1 | Acc]);
        <<_:O/binary, C1, C2, C3, _/binary>> when C1 >= 224, C1 =< 239,
                C2 >= 128, C2 =< 191,
                C3 >= 128, C3 =< 191 ->
            tokenize_string(B, ?ADV_COL(S, 3), [C3, C2, C1 | Acc]);
        <<_:O/binary, C1, C2, C3, C4, _/binary>> when C1 >= 240, C1 =< 244,
                C2 >= 128, C2 =< 191,
                C3 >= 128, C3 =< 191,
                C4 >= 128, C4 =< 191 ->
            tokenize_string(B, ?ADV_COL(S, 4), [C4, C3, C2, C1 | Acc]);
        _ ->
            throw(invalid_utf8)
    end.

tokenize_number(B, S) ->
    case tokenize_number(B, sign, S, []) of
        {{int, Int}, S1} ->
            {{const, list_to_integer(Int)}, S1};
        {{float, Float}, S1} ->
            {{const, list_to_float(Float)}, S1}
    end.

tokenize_number(B, sign, S=#decoder{offset=O}, []) ->
    case B of
        <<_:O/binary, $-, _/binary>> ->
            tokenize_number(B, int, ?INC_COL(S), [$-]);
        _ ->
            tokenize_number(B, int, S, [])
    end;
tokenize_number(B, int, S=#decoder{offset=O}, Acc) ->
    case B of
        <<_:O/binary, $0, _/binary>> ->
            tokenize_number(B, frac, ?INC_COL(S), [$0 | Acc]);
        <<_:O/binary, C, _/binary>> when C >= $1 andalso C =< $9 ->
            tokenize_number(B, int1, ?INC_COL(S), [C | Acc])
    end;
tokenize_number(B, int1, S=#decoder{offset=O}, Acc) ->
    case B of
        <<_:O/binary, C, _/binary>> when C >= $0 andalso C =< $9 ->
            tokenize_number(B, int1, ?INC_COL(S), [C | Acc]);
        _ ->
            tokenize_number(B, frac, S, Acc)
    end;
tokenize_number(B, frac, S=#decoder{offset=O}, Acc) ->
    case B of
        <<_:O/binary, $., C, _/binary>> when C >= $0, C =< $9 ->
            tokenize_number(B, frac1, ?ADV_COL(S, 2), [C, $. | Acc]);
        <<_:O/binary, E, _/binary>> when E =:= $e orelse E =:= $E ->
            tokenize_number(B, esign, ?INC_COL(S), [$e, $0, $. | Acc]);
        _ ->
            {{int, lists:reverse(Acc)}, S}
    end;
tokenize_number(B, frac1, S=#decoder{offset=O}, Acc) ->
    case B of
        <<_:O/binary, C, _/binary>> when C >= $0 andalso C =< $9 ->
            tokenize_number(B, frac1, ?INC_COL(S), [C | Acc]);
        <<_:O/binary, E, _/binary>> when E =:= $e orelse E =:= $E ->
            tokenize_number(B, esign, ?INC_COL(S), [$e | Acc]);
        _ ->
            {{float, lists:reverse(Acc)}, S}
    end;
tokenize_number(B, esign, S=#decoder{offset=O}, Acc) ->
    case B of
        <<_:O/binary, C, _/binary>> when C =:= $- orelse C=:= $+ ->
            tokenize_number(B, eint, ?INC_COL(S), [C | Acc]);
        _ ->
            tokenize_number(B, eint, S, Acc)
    end;
tokenize_number(B, eint, S=#decoder{offset=O}, Acc) ->
    case B of
        <<_:O/binary, C, _/binary>> when C >= $0 andalso C =< $9 ->
            tokenize_number(B, eint1, ?INC_COL(S), [C | Acc])
    end;
tokenize_number(B, eint1, S=#decoder{offset=O}, Acc) ->
    case B of
        <<_:O/binary, C, _/binary>> when C >= $0 andalso C =< $9 ->
            tokenize_number(B, eint1, ?INC_COL(S), [C | Acc]);
        _ ->
            {{float, lists:reverse(Acc)}, S}
    end.

tokenize(B, S=#decoder{offset=O}) ->
    case B of
        <<_:O/binary, C, _/binary>> when ?IS_WHITESPACE(C) ->
            tokenize(B, ?INC_CHAR(S, C));
        <<_:O/binary, "{", _/binary>> ->
            {start_object, ?INC_COL(S)};
        <<_:O/binary, "}", _/binary>> ->
            {end_object, ?INC_COL(S)};
        <<_:O/binary, "[", _/binary>> ->
            {start_array, ?INC_COL(S)};
        <<_:O/binary, "]", _/binary>> ->
            {end_array, ?INC_COL(S)};
        <<_:O/binary, ",", _/binary>> ->
            {comma, ?INC_COL(S)};
        <<_:O/binary, ":", _/binary>> ->
            {colon, ?INC_COL(S)};
        <<_:O/binary, "null", _/binary>> ->
            {{const, null}, ?ADV_COL(S, 4)};
        <<_:O/binary, "true", _/binary>> ->
            {{const, true}, ?ADV_COL(S, 4)};
        <<_:O/binary, "false", _/binary>> ->
            {{const, false}, ?ADV_COL(S, 5)};
        <<_:O/binary, "\"", _/binary>> ->
            tokenize_string(B, ?INC_COL(S));
        <<_:O/binary, C, _/binary>> when (C >= $0 andalso C =< $9)
                                         orelse C =:= $- ->
            tokenize_number(B, S);
        <<_:O/binary>> ->
            trim = S#decoder.state,
            {eof, S}
    end.

%% @spec digits(number()) -> string()
%% @doc  Returns a string that accurately represents the given integer or float
%%       using a conservative amount of digits. Great for generating
%%       human-readable output, or compact ASCII serializations for floats.
digits(N) when is_integer(N) ->
    integer_to_list(N);
digits(0.0) ->
    "0.0";
digits(Float) ->
    {Frac1, Exp1} = frexp_int(Float),
    [Place0 | Digits0] = digits1(Float, Exp1, Frac1),
    {Place, Digits} = transform_digits(Place0, Digits0),
    R = insert_decimal(Place, Digits),
    case Float < 0 of
        true ->
            [$- | R];
        _ ->
            R
    end.

%% @spec frexp(F::float()) -> {Frac::float(), Exp::float()}
%% @doc  Return the fractional and exponent part of an IEEE 754 double,
%%       equivalent to the libc function of the same name.
%%       F = Frac * pow(2, Exp).
frexp(F) ->
    frexp1(unpack(F)).

%% @spec int_pow(X::integer(), N::integer()) -> Y::integer()
%% @doc  Moderately efficient way to exponentiate integers.
%%       int_pow(10, 2) = 100.
int_pow(_X, 0) ->
    1;
int_pow(X, N) when N > 0 ->
    int_pow(X, N, 1).

%% @spec int_ceil(F::float()) -> integer()
%% @doc  Return the ceiling of F as an integer. The ceiling is defined as
%%       F when F == trunc(F);
%%       trunc(F) when F &lt; 0;
%%       trunc(F) + 1 when F &gt; 0.
int_ceil(X) ->
    T = trunc(X),
    case (X - T) of
        Pos when Pos > 0 -> T + 1;
        _ -> T
    end.


%% Internal API

int_pow(X, N, R) when N < 2 ->
    R * X;
int_pow(X, N, R) ->
    int_pow(X * X, N bsr 1, case N band 1 of 1 -> R * X; 0 -> R end).

insert_decimal(0, S) ->
    "0." ++ S;
insert_decimal(Place, S) when Place > 0 ->
    L = length(S),
    case Place - L of
         0 ->
            S ++ ".0";
        N when N < 0 ->
            {S0, S1} = lists:split(L + N, S),
            S0 ++ "." ++ S1;
        N when N < 6 ->
            %% More places than digits
            S ++ lists:duplicate(N, $0) ++ ".0";
        _ ->
            insert_decimal_exp(Place, S)
    end;
insert_decimal(Place, S) when Place > -6 ->
    "0." ++ lists:duplicate(abs(Place), $0) ++ S;
insert_decimal(Place, S) ->
    insert_decimal_exp(Place, S).

insert_decimal_exp(Place, S) ->
    [C | S0] = S,
    S1 = case S0 of
             [] ->
                 "0";
             _ ->
                 S0
         end,
    Exp = case Place < 0 of
              true ->
                  "e-";
              false ->
                  "e+"
          end,
    [C] ++ "." ++ S1 ++ Exp ++ integer_to_list(abs(Place - 1)).


digits1(Float, Exp, Frac) ->
    Round = ((Frac band 1) =:= 0),
    case Exp >= 0 of
        true ->
            BExp = 1 bsl Exp,
            case (Frac =/= ?BIG_POW) of
                true ->
                    scale((Frac * BExp * 2), 2, BExp, BExp,
                          Round, Round, Float);
                false ->
                    scale((Frac * BExp * 4), 4, (BExp * 2), BExp,
                          Round, Round, Float)
            end;
        false ->
            case (Exp =:= ?MIN_EXP) orelse (Frac =/= ?BIG_POW) of
                true ->
                    scale((Frac * 2), 1 bsl (1 - Exp), 1, 1,
                          Round, Round, Float);
                false ->
                    scale((Frac * 4), 1 bsl (2 - Exp), 2, 1,
                          Round, Round, Float)
            end
    end.

scale(R, S, MPlus, MMinus, LowOk, HighOk, Float) ->
    Est = int_ceil(math:log10(abs(Float)) - 1.0e-10),
    %% Note that the scheme implementation uses a 326 element look-up table
    %% for int_pow(10, N) where we do not.
    case Est >= 0 of
        true ->
            fixup(R, S * int_pow(10, Est), MPlus, MMinus, Est,
                  LowOk, HighOk);
        false ->
            Scale = int_pow(10, -Est),
            fixup(R * Scale, S, MPlus * Scale, MMinus * Scale, Est,
                  LowOk, HighOk)
    end.

fixup(R, S, MPlus, MMinus, K, LowOk, HighOk) ->
    TooLow = case HighOk of
                 true ->
                     (R + MPlus) >= S;
                 false ->
                     (R + MPlus) > S
             end,
    case TooLow of
        true ->
            [(K + 1) | generate(R, S, MPlus, MMinus, LowOk, HighOk)];
        false ->
            [K | generate(R * 10, S, MPlus * 10, MMinus * 10, LowOk, HighOk)]
    end.

generate(R0, S, MPlus, MMinus, LowOk, HighOk) ->
    D = R0 div S,
    R = R0 rem S,
    TC1 = case LowOk of
              true ->
                  R =< MMinus;
              false ->
                  R < MMinus
          end,
    TC2 = case HighOk of
              true ->
                  (R + MPlus) >= S;
              false ->
                  (R + MPlus) > S
          end,
    case TC1 of
        false ->
            case TC2 of
                false ->
                    [D | generate(R * 10, S, MPlus * 10, MMinus * 10,
                                  LowOk, HighOk)];
                true ->
                    [D + 1]
            end;
        true ->
            case TC2 of
                false ->
                    [D];
                true ->
                    case R * 2 < S of
                        true ->
                            [D];
                        false ->
                            [D + 1]
                    end
            end
    end.

unpack(Float) ->
    <<Sign:1, Exp:11, Frac:52>> = <<Float:64/float>>,
    {Sign, Exp, Frac}.

frexp1({_Sign, 0, 0}) ->
    {0.0, 0};
frexp1({Sign, 0, Frac}) ->
    Exp = log2floor(Frac),
    <<Frac1:64/float>> = <<Sign:1, ?FLOAT_BIAS:11, (Frac-1):52>>,
    {Frac1, -(?FLOAT_BIAS) - 52 + Exp};
frexp1({Sign, Exp, Frac}) ->
    <<Frac1:64/float>> = <<Sign:1, ?FLOAT_BIAS:11, Frac:52>>,
    {Frac1, Exp - ?FLOAT_BIAS}.

log2floor(Int) ->
    log2floor(Int, 0).

log2floor(0, N) ->
    N;
log2floor(Int, N) ->
    log2floor(Int bsr 1, 1 + N).


transform_digits(Place, [0 | Rest]) ->
    transform_digits(Place, Rest);
transform_digits(Place, Digits) ->
    {Place, [$0 + D || D <- Digits]}.


frexp_int(F) ->
    case unpack(F) of
        {_Sign, 0, Frac} ->
            {Frac, ?MIN_EXP};
        {_Sign, Exp, Frac} ->
            {Frac + (1 bsl 52), Exp - 53 - ?FLOAT_BIAS}
    end.
