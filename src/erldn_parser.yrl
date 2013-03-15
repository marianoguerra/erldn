Nonterminals
    value list list_items vector set map key_value_pairs key_value_pair tagged.

Terminals
    float integer boolean string nil open_list close_list open_vector
    close_vector open_map close_map sharp ignore keyword symbol char.

Rootsymbol value.

list_items -> value : ['$1'].
list_items -> value list_items : ['$1'|'$2'].

list -> open_list close_list : [].
list -> open_list list_items close_list : '$2'.

vector -> open_vector close_vector : {vector, []}.
vector -> open_vector list_items close_vector : {vector, '$2'}.

set -> sharp open_map close_map : {set, []}.
set -> sharp open_map list_items close_map : {set, '$3'}.

key_value_pair -> value value : {'$1', '$2'}.

key_value_pairs -> key_value_pair : ['$1'].
key_value_pairs -> key_value_pair key_value_pairs : ['$1'|'$2'].

map -> open_map close_map : {map, []}.
map -> open_map key_value_pairs close_map : {map, '$2'}.

tagged -> sharp symbol value : {tag, unwrap('$2'), '$3'}.

value -> nil     : unwrap('$1').
value -> float   : unwrap('$1').
value -> integer : unwrap('$1').
value -> boolean : unwrap('$1').
value -> string  : unwrap('$1').
value -> list    : '$1'.
value -> vector  : '$1'.
value -> set     : '$1'.
value -> map     : '$1'.
value -> keyword : 
    Keyword = unwrap('$1'),
    if
        Keyword == nil -> {keyword, nil};
        true -> Keyword
    end.
value -> symbol  : {symbol, unwrap('$1')}.
value -> tagged  : '$1'.
value -> char    : {char, unwrap('$1')}.
value -> ignore value : {ignore, '$2'}.

Erlang code.

unwrap({_,V})   -> V;
unwrap({_,_,V}) -> V.
