-module(erldn).
-export([lex_str/1, parse_str/1, to_string/1, to_erlang/1, to_erlang/2]).

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

keyvals_to_string(Items) -> keyvals_to_string(Items, []).

keyvals_to_string([], Accum) -> lists:reverse(Accum);
keyvals_to_string([{K, V}], Accum) -> 
    keyvals_to_string([], [to_string(V), " ", to_string(K)|Accum]);
keyvals_to_string([{K, V}|T], Accum) ->
    keyvals_to_string(T, [" ", to_string(V), " ", to_string(K)|Accum]).

items_to_string(Items) -> items_to_string(Items, []).

items_to_string([], Accum) -> lists:reverse(Accum);
items_to_string([H], Accum) -> 
    items_to_string([], [to_string(H)|Accum]);
items_to_string([H|T], Accum) ->
    items_to_string(T, [(to_string(H) ++ " ")|Accum]).

to_string(Edn) -> lists:reverse(to_string(Edn, [])).

to_string(Value, Accum) when is_binary(Value) ->
    ["\"", escape_string(binary_to_list(Value)), "\""|Accum];
to_string({symbol, Symbol}, Accum) -> [atom_to_list(Symbol)|Accum];
to_string({keyword, nil}, Accum) -> [":nil"|Accum];
to_string({char, C}, Accum) -> [["\\"|[C]]|Accum];
to_string({vector, Items}, Accum) -> ["]", items_to_string(Items), "["|Accum];
to_string({set, Items}, Accum) -> ["}", items_to_string(Items), "#{"|Accum];
to_string({map, Items}, Accum) -> ["}", keyvals_to_string(Items), "{"|Accum];
to_string(Items, Accum) when is_list(Items) -> [")", items_to_string(Items), "("|Accum];
to_string(true, Accum) -> ["true"|Accum];
to_string(false, Accum) -> ["false"|Accum];
to_string(nil, Accum) -> ["nil"|Accum];
to_string(Item, Accum) when is_atom(Item) -> [atom_to_list(Item), ":"|Accum];
to_string({tag, Tag, Value}, Accum) -> [to_string(Value), " ", atom_to_list(Tag), "#"|Accum];
to_string(Value, Accum) -> [io_lib:format("~p", [Value])|Accum].

escape_string(String) -> escape_string(String, []).

escape_string([], Output) ->
  lists:reverse(Output);

escape_string([Char|Rest], Output) ->
  Chars = map_escaped_char(Char),
  escape_string(Rest, [Chars|Output]).

map_escaped_char(Char) ->
  case Char of
    $\\ -> [$\\, $\\];
    $\" -> [$\\, $\"];
    $\n -> [$\\, $n];
    $\r -> [$\\, $r];
    $\t -> [$\\, $t];
    _ -> Char
  end.

key_vals_to_erlang({Key, Val}, Handlers) ->
    {to_erlang(Key, Handlers), to_erlang(Val, Handlers)}.

to_erlang(Val) -> to_erlang(Val, []).

to_erlang({char, Char}, _Handlers) -> unicode:characters_to_binary([Char], utf8);
to_erlang({keyword, nil}, _Handlers) -> nil;
to_erlang({vector, Items}, Handlers) -> to_erlang(Items, Handlers);
to_erlang({set, Items}, Handlers) -> sets:from_list(to_erlang(Items, Handlers));
to_erlang({map, Kvs}, Handlers) ->
    dict:from_list(lists:map(fun (V) -> key_vals_to_erlang(V, Handlers) end, Kvs));
to_erlang(Val, Handlers) when is_list(Val) ->
    lists:map(fun (V) -> to_erlang(V, Handlers) end, Val);
to_erlang({tag, Tag, Val}, Handlers) ->
    Result = lists:keyfind(Tag, 1, Handlers),

    if
        Result == false -> throw({handler_not_found_for_tag, Tag});
        true ->
            {_, Handler} = Result,
            Handler(Tag, Val, Handlers)
    end;

to_erlang(Val, _Handlers) -> Val.
