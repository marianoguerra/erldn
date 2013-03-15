Definitions.

Bool = (true|false)
Nil  = nil

% numbers
Number      = [0-9]
Float       = [0-9]+\.[0-9]+([eE][-+]?[0-9]+)?

% delimiters and operators
OpenList    = \(
CloseList   = \)
OpenMap     = {
CloseMap    = }
OpenVector  = \[
CloseVector = \]
Whites      = [\s|,\n]+
Sharp       = #
Symbol      = [\.\*\+\!\-\_\?\$%&=a-zA-Z][\.\*\+\!\-\_\?\$%&=a-zA-Z0-9:#]*
Slash       = /
Colon       = :
Comments    = ;.*\n

% string stuff
String      = "(\\\^.|\\.|[^\"])*"

Rules.

% numbers
{Float}                  : make_token(float, TokenLine, TokenChars, fun erlang:list_to_float/1).
{Number}+                : make_token(integer, TokenLine, TokenChars, fun parse_number/1).

% delimiters and operators
{OpenList}               : make_token(open_list, TokenLine, TokenChars).
{CloseList}              : make_token(close_list, TokenLine, TokenChars).
{OpenMap}                : make_token(open_map, TokenLine, TokenChars).
{CloseMap}               : make_token(close_map, TokenLine, TokenChars).
{OpenVector}             : make_token(open_vector, TokenLine, TokenChars).
{CloseVector}            : make_token(close_vector, TokenLine, TokenChars).

% string stuff
{String}                 : build_string(string, TokenChars, TokenLine, TokenLen).

% identifiers and atoms
{Bool}                   : make_token(boolean, TokenLine, TokenChars).
{Nil}                    : make_token(nil, TokenLine, TokenChars).
{Sharp}_                 : make_token(ignore, TokenLine, TokenChars).
{Sharp}                  : make_token(sharp, TokenLine, TokenChars).
{Symbol}                 : make_token(symbol, TokenLine, TokenChars).
{Slash}                  : make_token(symbol, TokenLine, TokenChars).
{Slash}{Symbol}          : make_token(symbol, TokenLine, TokenChars).
{Symbol}{Slash}{Symbol}  : make_token(symbol, TokenLine, TokenChars).

{Colon}{Symbol}                 : make_token(keyword, TokenLine, tl(TokenChars)).
{Colon}{Slash}                  : make_token(keyword, TokenLine, tl(TokenChars)).
{Colon}{Slash}{Symbol}          : make_token(keyword, TokenLine, tl(TokenChars)).
{Colon}{Symbol}{Slash}{Symbol}  : make_token(keyword, TokenLine, tl(TokenChars)).

{Whites}                : skip_token.
{Comments}              : skip_token.

Erlang code.

make_token(Name, Line, Chars) when is_list(Chars) ->
    {token, {Name, Line, list_to_atom(Chars)}};
make_token(Name, Line, Chars) ->
    {token, {Name, Line, Chars}}.

make_token(Name, Line, Chars, Fun) ->
    {token, {Name, Line, Fun(Chars)}}.

build_string(Type, Str, Line, _Len) ->
  StrLen = length(Str),
  StringContent = lists:sublist(Str, 2, StrLen - 2),
  String = binary:list_to_bin(unescape_string(StringContent, Line)),
  {token, {Type, Line, String}}.

parse_number(Str) ->
    list_to_integer(Str).

unescape_string(String, Line) -> unescape_string(String, Line, []).

unescape_string([], _Line, Output) ->
  lists:reverse(Output);
unescape_string([$\\, Escaped | Rest], Line, Output) ->
  Char = map_escaped_char(Escaped, Line),
  unescape_string(Rest, Line, [Char|Output]);
unescape_string([Char|Rest], Line, Output) ->
  unescape_string(Rest, Line, [Char|Output]).

map_escaped_char(Escaped, Line) ->
  case Escaped of
    $\\ -> $\\;
    $/ -> $/;
    $\" -> $\";
    $\' -> $\';
    $n -> $\n;
    $r -> $\r;
    $t -> $\t;
    _ -> throw({error, {Line, fng_lexer, ["unrecognized escape sequence: ", [$\\, Escaped]]}})
  end.
