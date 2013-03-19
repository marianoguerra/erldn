-module(erldn_tests).
-include_lib("eunit/include/eunit.hrl").

check(Val, Str) ->
    Result = lists:flatten(erldn:to_string(Val)),
    ?assertEqual(Str, Result).

cte(Val, Expected) ->
    Result = erldn:to_erlang(Val),
    ?assertEqual(Expected, Result).

cte(Val, Expected, Transformer) ->
    Result = Transformer(erldn:to_erlang(Val)),
    ?assertEqual(Expected, Result).

char_to_string_test() -> check({char, $$}, "\\$").
nil_keyword_to_string_test() -> check({keyword, nil}, ":nil").
keyword_to_string_test() -> check(foo, ":foo").
true_to_string_test() -> check(true, "true").
false_to_string_test() -> check(false, "false").
nil_to_string_test() -> check(nil, "nil").
number_to_string_test() -> check(42, "42").
float_to_string_test() -> check(42.4, "42.4").
string_to_string_test() -> check(<<"hello">>, "\"hello\"").
string_with_escapes_to_string_test() -> check(<<"h\n\r\"\\ello">>,
                                              "\"h\\n\\r\\\"\\\\ello\"").
symbol_to_string_test() -> check({symbol, foo}, "foo").
tagged_value_to_string_test() -> check({tag, foo, 42}, "#foo 42").

empty_vector_to_string_test() -> check({vector, []}, "[]").
empty_list_to_string_test() -> check([], "()").
empty_set_to_string_test() -> check({set, []}, "#{}").
empty_map_to_string_test() -> check({map, []}, "{}").

one_item_map_to_string_test() -> check({map, [{true, 1}]}, "{true 1}").
one_item_vector_to_string_test() -> check({vector, [1]}, "[1]").
one_item_list_to_string_test() -> check([1], "(1)").
one_item_set_to_string_test() -> check({set, [1]}, "#{1}").

vector_to_string_test() -> check({vector, [1, foo]}, "[1 :foo]").
list_to_string_test() -> check([1, foo], "(1 :foo)").
set_to_string_test() -> check({set, [1, foo]}, "#{1 :foo}").
map_to_string_test() -> check({map, [{1, foo}, {2, bar}]}, "{1 :foo 2 :bar}").

vector3_to_string_test() -> check({vector, [1, foo, nil]}, "[1 :foo nil]").
list3_to_string_test() -> check([1, foo, nil], "(1 :foo nil)").
set3_to_string_test() -> check({set, [1, foo, nil]}, "#{1 :foo nil}").
map3_to_string_test() -> check({map, [{1, foo}, {2, bar}, {3, nil}]},
                               "{1 :foo 2 :bar 3 nil}").


char_to_erlang_test() -> cte({char, $a}, <<"a">>).
integer_to_erlang_test() -> cte(41, 41).
float_to_erlang_test() -> cte(41.2, 41.2).
keyword_to_erlang_test() -> cte(foo, foo).
nil_to_erlang_test() -> cte(nil, nil).
true_to_erlang_test() -> cte(true, true).
false_to_erlang_test() -> cte(false, false).
string_to_erlang_test() -> cte(<<"asd">>, <<"asd">>).

list_to_erlang_test() -> cte([], []).
list1_to_erlang_test() -> cte([1], [1]).
list2_to_erlang_test() -> cte([1, foo], [1, foo]).
list_nested_to_erlang_test() -> cte([1, {char, $a}], [1, <<"a">>]).

vector_to_erlang_test() -> cte({vector, []}, []).
vector1_to_erlang_test() -> cte({vector, [1]}, [1]).
vector2_to_erlang_test() -> cte({vector, [1, foo]}, [1, foo]).
vector_nested_to_erlang_test() -> cte({vector, [1, {char, $a}]}, [1, <<"a">>]).

set_nested_to_erlang_test() -> cte({set, [1, {char, $a}, 1]}, [1, <<"a">>],
                                   fun sets:to_list/1).
map_to_erlang_test() -> cte({map, [{1, {char, $a}}]}, [{1, <<"a">>}],
                                   fun dict:to_list/1).
nil_keyword_to_erlang_test() -> cte({keyword, nil}, nil).
symbol_to_erlang_test() -> cte({symbol, foo}, {symbol, foo}).
tag_to_erlang_test() ->
    Result = erldn:to_erlang({tag, foo, 42}, [{foo, fun (_, Val, _) -> Val + 1 end}]),
    ?assertEqual(43, Result).

unknown_tag_raises_in_to_erlang_test() ->
    try
        erldn:to_erlang({tag, bar, 42}, [{foo, fun (_, Val, _) -> Val + 1 end}]),
        throw(should_fail)
    catch
        {handler_not_found_for_tag, bar} -> ok
    end.

