-module(erldn_parser_tests).
-include_lib("eunit/include/eunit.hrl").

check(Str, Expected) ->
    {ok, Result} = erldn:parse_str(Str),
    ?assertEqual(Expected, Result).

integer_test() -> check("1", 1).
integer_big_test() -> check("1234", 1234).

float_test() -> check("1.3", 1.3).
float_big_test() -> check("1.234", 1.234).

bool_true_test() -> check("true", true).
bool_false_test() -> check("false", false).
bool_nil_test() -> check("nil", nil).

string_test() -> check("\"hello\"", <<"hello">>).
empty_string_test() -> check("\"\"", <<"">>).

empty_list_test() -> check("()", []).
one_item_list_test() -> check("(1)", [1]).
two_item_list_test() -> check("(1 true)", [1, true]).
three_item_list_test() -> check("(1 true nil)", [1, true, nil]).
two_item_list_with_commas_test() -> check("(1, true)", [1, true]).
three_item_list_with_commas_test() -> check("(1, true, nil)", [1, true, nil]).
nested_list_test() -> check("(1, (true, nil), 1.2)", [1, [true, nil], 1.2]).

empty_vector_test() -> check("[]", {vector, []}).
one_item_vector_test() -> check("[1]", {vector, [1]}).
two_item_vector_test() -> check("[1 true]", {vector, [1, true]}).
three_item_vector_test() -> check("[1 true nil]", {vector, [1, true, nil]}).
two_item_vector_with_commas_test() -> check("[1, true]", {vector, [1, true]}).
three_item_vector_with_commas_test() -> check("[1, true, nil]", {vector, [1, true, nil]}).
nested_vector_test() -> check("[1, (true, nil), 1.2]", {vector, [1, [true, nil], 1.2]}).

empty_set_test() -> check("#{}", {set, []}).
one_item_set_test() -> check("#{1}", {set, [1]}).
two_item_set_test() -> check("#{1 true}", {set, [1, true]}).
three_item_set_test() -> check("#{1 true nil}", {set, [1, true, nil]}).
two_item_set_with_commas_test() -> check("#{1, true}", {set, [1, true]}).
three_item_set_with_commas_test() -> check("#{1, true, nil}", {set, [1, true, nil]}).
nested_set_test() -> check("#{1, (true, nil), 1.2}", {set, [1, [true, nil], 1.2]}).

empty_map_test() -> check("{}", {map, []}).
one_item_map_test() -> check("{1 true}", {map, [{1, true}]}).
two_items_map_test() -> check("{1 true, false nil}",
                              {map, [{1, true}, {false, nil}]}).
three_items_map_test() -> check("{1 true, false nil, \"key\" 42}",
                                {map, [{1, true}, {false, nil}, {<<"key">>, 42}]}).
nested_items_map_test() -> check("{1 (true), false [nil], \"key\" #{42}}",
                                {map, [{1, [true]},
                                       {false, {vector, [nil]}},
                                       {<<"key">>, {set, [42]}}]}).

simple_symbol_test() -> check("foo", {symbol, foo}).
slash_symbol_test() -> check("/", {symbol, '/'}).
start_with_slash_symbol_test() -> check("/foo", {symbol, '/foo'}).
ns_symbol_test() -> check("ns/foo", {symbol, 'ns/foo'}).
ns1_symbol_test() -> check("org.marianoguerra/erldn",
                           {symbol, 'org.marianoguerra/erldn'}).

simple_keyword_test() -> check(":foo", foo).
nil_keyword_test() -> check(":nil", {keyword, nil}).
number_keyword_test() -> check(":300x450", '300x450').
slash_keyword_test() -> check(":/", '/').
start_with_slash_keyword_test() -> check(":/foo", '/foo').
ns_keyword_test() -> check(":ns/foo", 'ns/foo').
ns1_keyword_test() -> check(":org.marianoguerra/erldn", 'org.marianoguerra/erldn').

simple_tag_test() -> check("#answer 42", {tag, answer, 42}).
map_tag_test() -> check("#myapp/Person {:first \"Fred\" :last \"Mertz\"}",
                        {tag, 'myapp/Person', {map, [{first, <<"Fred">>},
                                                     {last, <<"Mertz">>}]}}).
instant_test() -> check("#inst \"1985-04-12T23:20:50.52Z\"",
                        {tag, inst, <<"1985-04-12T23:20:50.52Z">>}).
uuid_test() -> check("#uuid \"f81d4fae-7dec-11d0-a765-00a0c91e6bf6\"",
                     {tag, uuid, <<"f81d4fae-7dec-11d0-a765-00a0c91e6bf6">>}).

ignore_next_test() -> check("#_ 4", {ignore, 4}).
ignore_next_number_no_space_test() -> check("#_4", {ignore, 4}).
ignore_next_atom_no_space_test() -> check("#_foo", {ignore, {symbol, foo}}).

char_test() -> check("\\c", {char, $c}).
char1_test() -> check("\\D", {char, $D}).
char_newline_test() -> check("\\newline", {char, $\n}).
char_tab_test() -> check("\\tab", {char, $\t}).
char_space_test() -> check("\\space", {char, 32}).
char_return_test() -> check("\\return", {char, $\r}).

comment_test() -> check("{1 ; comment \ntrue}", {map, [{1, true}]}).
