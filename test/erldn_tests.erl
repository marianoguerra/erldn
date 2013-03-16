-module(erldn_tests).
-include_lib("eunit/include/eunit.hrl").

check(Val, Str) ->
    Result = lists:flatten(erldn:to_string(Val)),
    ?assertEqual(Str, Result).

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
