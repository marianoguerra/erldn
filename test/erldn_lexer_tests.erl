-module(erldn_lexer_tests).
-include_lib("eunit/include/eunit.hrl").

check(Str, Expected) ->
    {ok, [Result], _} = erldn:lex_str(Str),
    ?assertEqual(Expected, Result).

integer_test() -> check("1", {integer, 1, 1}).
integer_big_test() -> check("1234", {integer, 1, 1234}).

float_test() -> check("1.3", {float, 1, 1.3}).
float_big_test() -> check("1.234", {float, 1, 1.234}).

bool_true_test() -> check("true", {boolean, 1, true}).
bool_false_test() -> check("false", {boolean, 1, false}).
bool_nil_test() -> check("nil", {nil, 1, nil}).

string_test() -> check("\"hello\"", {string, 1, "hello"}).
empty_string_test() -> check("\"\"", {string, 1, ""}).

sharp_test() -> check("#", {sharp, 1, '#'}).

simple_symbol_test() -> check("foo", {symbol, 1, foo}).
slash_symbol_test() -> check("/", {symbol, 1, '/'}).
start_with_slash_symbol_test() -> check("/foo", {symbol, 1, '/foo'}).
ns_symbol_test() -> check("ns/foo", {symbol, 1, 'ns/foo'}).
ns1_symbol_test() -> check("org.marianoguerra/erldn",
                           {symbol, 1, 'org.marianoguerra/erldn'}).

simple_keyword_test() -> check(":foo", {keyword, 1, foo}).
slash_keyword_test() -> check(":/", {keyword, 1, '/'}).
start_with_slash_keyword_test() -> check(":/foo", {keyword, 1, '/foo'}).
ns_keyword_test() -> check(":ns/foo", {keyword, 1, 'ns/foo'}).
ns1_keyword_test() -> check(":org.marianoguerra/erldn",
                           {keyword, 1, 'org.marianoguerra/erldn'}).

%char_test() -> check("\c", {char, 1, $c}).
%char1_test() -> check("\D", {char, 1, $D}).
%char_newline_test() -> check("\newline", {char, 1, $\n}).
%char_tab_test() -> check("\tab", {char, 1, $\t}).
%char_space_test() -> check("\space", {char, 1, 32}).
%char_return_test() -> check("\return", {char, 1, $\r}).

ignore_token_test() -> check("#_", {ignore, 1, '#_'}).
