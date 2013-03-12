-module(erldn).
-export([lex_str/1, parse_str/1]).

lex_str(Str) -> erldn_lexer:string(Str).

parse_str(Str) ->
    case lex_str(Str) of
        {ok, Tokens, _} ->
            case erldn_parser:parse(Tokens) of
                {ok, Tree}    -> {ok, Tree};
                {ok, Tree, _Warns} -> {ok, Tree};
                {error, Error} -> {error, Error, nil};
                {error, Warns, Errors} -> {error, Errors, Warns}
            end;
        Error -> Error
    end.
