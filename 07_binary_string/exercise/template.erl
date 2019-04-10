-module(template).

-export([parse/2, parse2/2]).


parse(Str, Data) when is_binary(Str) ->
    Str2 = binary:split(Str, [<<"{{">>], [global]),
    parse2(Str2, Data).


parse2(Str, Data) -> parse2(Str, Data, []).

parse2([], _, Acc) -> unicode:characters_to_binary(lists:reverse(Acc));

parse2([H | D], Data, Acc) -> 
    case binary:split(H, [<<"}}">>]) of
        [One] -> parse2(D, Data, [One | Acc]);
        [Second | Rest] -> 
            case maps:find(Second, Data) of
                error -> parse2(D, Data, [Rest |Acc]);
                {ok, Value} when is_binary(Value) ->
                    parse2(D, Data, [Rest, Value | Acc]);
                {ok, Value} when is_list(Value) ->
                    parse2(D, Data, [Rest, unicode:characters_to_binary(Value)| Acc]);
                {ok, Value} when is_integer(Value) ->
                    parse2(D, Data, [Rest, integer_to_binary(Value) | Acc])
            end
    end.    
