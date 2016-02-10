-module(erldn_lexer_tests).
-include_lib("eunit/include/eunit.hrl").

check(Str, Expected) ->
    {ok, [Result], _} = erldn:lex_str(Str),
    ?assertEqual(Expected, Result).

integer_test() -> check("1", {integer, 1, 1}).
integer_big_test() -> check("1234", {integer, 1, 1234}).
integer_plus_test() -> check("+1234", {integer, 1, 1234}).
integer_minus_test() -> check("-1234", {integer, 1, -1234}).
integer_minus_zero_test() -> check("-0", {integer, 1, 0}).
integer_arbitrary_precision_test() -> check("1234N", {integer, 1, 1234}).

float_test() -> check("1.3", {float, 1, 1.3}).
float_big_test() -> check("1.234", {float, 1, 1.234}).
float_plus_test() -> check("+1.234", {float, 1, 1.234}).
float_minus_test() -> check("-1.234", {float, 1, -1.234}).
float_exp_test() -> check("1.234E1", {float, 1, 12.34}).
float_plus_exp_test() -> check("1.234e+2", {float, 1, 123.4}).
float_neg_exp_test() -> check("-1.234e-1", {float, 1, -0.1234}).
float_exact_precision_test() -> check("1.234M", {float, 1, 1.234}).

bool_true_test() -> check("true", {boolean, 1, true}).
bool_false_test() -> check("false", {boolean, 1, false}).
bool_nil_test() -> check("nil", {nil, 1, nil}).

string_test() -> check("\"hello\"", {string, 1, <<"hello">>}).
empty_string_test() -> check("\"\"", {string, 1, <<"">>}).

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

char_test() -> check("\\c", {char, 1, $c}).
char1_test() -> check("\\D", {char, 1, $D}).
char2_test() -> check("\\$", {char, 1, $$}).
char_newline_test() -> check("\\newline", {char, 1, $\n}).
char_tab_test() -> check("\\tab", {char, 1, $\t}).
char_space_test() -> check("\\space", {char, 1, 32}).
char_return_test() -> check("\\return", {char, 1, $\r}).

char_in_a_string_test() -> check("\"hi \\c !\"", {string, 1, <<"hi \\c !">>}).

ignore_token_test() -> check("#_", {ignore, 1, '#_'}).

comment_test() -> check("1 ; this is a comment \n", {integer, 1, 1}).
comment_inside_string_test() -> check("\"; this is NOT a comment\n\"",
                                      {string, 1, <<"; this is NOT a comment\n">>}).
