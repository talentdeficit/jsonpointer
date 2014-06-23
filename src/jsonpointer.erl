%% The MIT License

%% Copyright (c) 2014 alisdair sullivan <alisdairsullivan@yahoo.ca>

%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:

%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.

%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%% THE SOFTWARE.

-module(jsonpointer).

-export([encode/1, decode/1]).
<<<<<<< HEAD
-export([get/2]).


get(Value, Pointer)
when is_list(Pointer); is_binary(Pointer) ->
    try Value of
        V when is_list(V) -> get_from_list(maybe_compile(Pointer), Value);
%        V when is_binary(V) -> get_from_json(maybe_compile(Pointer), Value);
        _ -> erlang:error(function_clause)
    catch error:_ -> erlang:error(badarg)
    end.


get_from_list([], Value) -> Value;
get_from_list([Ref|Rest], [{_,_}|_] = Value)
when is_binary(Ref) ->
    case proplists:get_value(Ref, Value) of
        undefined -> erlang:error(badarg);
        V -> get_from_list(Rest, V)
    end;
get_from_list([Ref|Rest], Value)
when is_integer(Ref) ->
    % jsonpointer arrays are zero indexed, erlang lists are indexed from 1
    try lists:nth(Ref + 1, Value) of
        V -> get_from_list(Rest, V)
    catch error:function_clause -> erlang:error(badarg)
    end;
get_from_list([Ref|Rest], Value) ->
    get_from_list([ref_to_int(Ref)] ++ Rest, Value).


maybe_compile(Pointer) when is_binary(Pointer) -> decode(Pointer);
maybe_compile(Pointer) -> Pointer.
=======
>>>>>>> parent of d1990b3... get/2 implementation and initial tests


encode(Refs) when is_list(Refs) -> encode(Refs, <<>>).

encode([], Bin) -> Bin;
encode([Ref|Rest], Bin)
when is_binary(Ref) ->
    encode(Rest, <<Bin/binary, $/, (escape(Ref))/binary>>);
encode([Ref|Rest], Bin)
when is_integer(Ref) ->
    IntBin = unicode:characters_to_binary(integer_to_list(Ref)),
    encode(Rest, <<Bin/binary, $/, IntBin/binary>>).


decode(Bin) -> decode(Bin, []).

decode(<<>>, Acc) -> lists:reverse(Acc);
decode(<<$~, $0, Rest/binary>>, [Current|Done]) ->
    decode(Rest, [<<Current/binary, $~>>] ++ Done);
decode(<<$~, $1, Rest/binary>>, [Current|Done]) ->
    decode(Rest, [<<Current/binary, $/>>] ++ Done);
decode(<<$/, Rest/binary>>, []) ->
    decode(Rest, [<<>>]);
decode(<<$/, Rest/binary>>, [Current|Done]) ->
    decode(Rest, [<<>>, Current] ++ Done);
decode(<<Codepoint/utf8, Rest/binary>>, [Current|Done]) ->
    decode(Rest, [<<Current/binary, Codepoint/utf8>>] ++ Done).


escape(Ref) -> escape(Ref, <<>>).

escape(<<>>, Acc) -> Acc;
escape(<<$~, Rest/binary>>, Acc) -> escape(Rest, <<Acc/binary, $~, $0>>);
escape(<<$/, Rest/binary>>, Acc) -> escape(Rest, <<Acc/binary, $~, $1>>);
escape(<<Codepoint/utf8, Rest/binary>>, Acc) -> escape(Rest, <<Acc/binary, Codepoint>>).


ref_to_int(<<"0">>) -> 0;
ref_to_int(<<Digit/utf8, _/binary>> = Ref)
when Digit >= 49, Digit =< 57 ->
    binary_to_integer(Ref).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

<<<<<<< HEAD
get_from_list_test_() ->
    List = [
        {<<"a">>, 1},
        {<<"b">>, [{<<"c">>, 2}]},
        {<<"d">>, [
            {<<"e">>, [
                [{<<"a">>, 3}], [{<<"b">>, 4}], [{<<"c">>, 5}]
            ]},
            {<<"e/f">>, 2}
        ]},
        {<<"a/b">>, [{<<"c">>, 1}]},
        {<<"~1">>, 3},
        {<<"01">>, 4}
    ],
    [
        ?_assertEqual(List, get(List, <<>>)),
        ?_assertEqual(List, get(List, [])),
        ?_assertEqual(1, get(List, <<"/a">>)),
        ?_assertEqual(1, get(List, [<<"a">>])),
        ?_assertEqual(2, get(List, <<"/b/c">>)),
        ?_assertEqual(2, get(List, [<<"b">>,<<"c">>])),
        ?_assertEqual(3, get(List, <<"/d/e/0/a">>)),
        ?_assertEqual(3, get(List, [<<"d">>,<<"e">>,<<"0">>,<<"a">>])),
        ?_assertEqual(3, get(List, [<<"d">>,<<"e">>,0,<<"a">>])),
        ?_assertEqual(4, get(List, <<"/d/e/1/b">>)),
        ?_assertEqual(4, get(List, [<<"d">>,<<"e">>,<<"1">>,<<"b">>])),
        ?_assertEqual(4, get(List, [<<"d">>,<<"e">>,1,<<"b">>])),
        ?_assertEqual(5, get(List, <<"/d/e/2/c">>)),
        ?_assertEqual(5, get(List, [<<"d">>,<<"e">>,<<"2">>,<<"c">>])),
        ?_assertEqual(5, get(List, [<<"d">>,<<"e">>,2,<<"c">>])),
        ?_assertError(badarg, get(List, <<"a">>)),
        ?_assertError(badarg, get(List, <<"a/">>))
    ].

rfc6901_examples_test_() ->
    Obj = [
        {<<"foo">>, [<<"bar">>, <<"baz">>]},
        {<<"">>, 0},
        {<<"a/b">>, 1},
        {<<"c%d">>, 2},
        {<<"e^f">>, 3},
        {<<"g|h">>, 4},
        {<<"i\\\\j">>, 5},
        {<<"k\\\"l">>, 6},
        {<<" ">>, 7},
        {<<"m~n">>, 8}
    ],
    [
        ?_assertEqual(Obj, get(Obj, <<"">>)),
        ?_assertEqual([<<"bar">>, <<"baz">>], get(Obj, <<"/foo">>)),
        ?_assertEqual(<<"bar">>, get(Obj, <<"/foo/0">>)),
        ?_assertEqual(0, get(Obj, <<"/">>)),
        ?_assertEqual(1, get(Obj, <<"/a~1b">>)),
        ?_assertEqual(2, get(Obj, <<"/c%d">>)),
        ?_assertEqual(3, get(Obj, <<"/e^f">>)),
        ?_assertEqual(4, get(Obj, <<"/g|h">>)),
        ?_assertEqual(5, get(Obj, <<"/i\\\\j">>)),
        ?_assertEqual(6, get(Obj, <<"/k\\\"l">>)),
        ?_assertEqual(7, get(Obj, <<"/ ">>)),
        ?_assertEqual(8, get(Obj, <<"/m~0n">>))
    ].

=======
>>>>>>> parent of d1990b3... get/2 implementation and initial tests
encode_test_() ->
    [
        {"root path", ?_assertEqual(<<>>, encode([]))},
        {"single reference", ?_assertEqual(
            <<"/foo">>,
            encode([<<"foo">>])
        )},
        {"multiple references", ?_assertEqual(
            <<"/foo/bar/baz">>,
            encode([<<"foo">>, <<"bar">>, <<"baz">>])
        )},
        {"integer reference", ?_assertEqual(
            <<"/foo/0/1/2">>,
            encode([<<"foo">>, 0, 1, 2])
        )},
        {"~ in reference", ?_assertEqual(
            <<"/~0/a~0a/~0foo/foo~0">>,
            encode([<<"~">>, <<"a~a">>, <<"~foo">>, <<"foo~">>])
        )},
        {"/ in reference", ?_assertEqual(
            <<"/~1/a~1a/~1foo/foo~1">>,
            encode([<<"/">>, <<"a/a">>, <<"/foo">>, <<"foo/">>])
        )}
    ].

decode_test_() ->
    [
        {"root path", ?_assertEqual([], decode(<<>>))},
        {"single reference", ?_assertEqual(
            [<<"foo">>],
            decode(<<"/foo">>)
        )},
        {"multiple references", ?_assertEqual(
            [<<"foo">>, <<"bar">>, <<"baz">>],
            decode(<<"/foo/bar/baz">>)
        )},
        {"~ in reference", ?_assertEqual(
            [<<"~">>, <<"a~a">>, <<"~foo">>, <<"foo~">>],
            decode(<<"/~0/a~0a/~0foo/foo~0">>)
        )},
        {"/ in reference", ?_assertEqual(
            [<<"/">>, <<"a/a">>, <<"/foo">>, <<"foo/">>],
            decode(<<"/~1/a~1a/~1foo/foo~1">>)
        )},
        {"~01 in reference", ?_assertEqual(
            [<<"~1">>],
            decode(<<"/~01">>)
        )}
    ].

-endif.