-module(tic_tac_toe).

-export([new_game/0, win/1, move/3, make_nested_tuple/1]).


new_game() ->
    {{f, f, f},
     {f, f, f},
     {f, f, f}}.


get_winner([]) -> no_win;
get_winner([{X, X, X} | _ ]) -> 
    case X of 
        f -> no_win;
        _ -> {win, X}
    end;
get_winner([_ | Rest]) -> get_winner(Rest).


win(GameState) ->
    {
        {A1, A2, A3},
        {B1, B2, B3},
        {C1, C2, C3}
    } = GameState,

    Rows = [{A1, A2, A3}, {B1, B2, B3}, {C1, C2, C3}],
    Columns = [{A1, B1, C1}, {A2, B2, C2}, {A3, B3, C3}],
    Diagonals = [{A1, B2, C3}, {A3, B2, C1}],

    get_winner(Rows ++ Columns ++ Diagonals).   



move(Cell, Player, GameState) ->
    Field = list_to_tuple(lists:flatten([tuple_to_list(X) || X <- tuple_to_list(GameState)])),
    try element(Cell, Field) of 
        f -> {ok, make_nested_tuple(setelement(Cell, Field, Player))};
        _ -> {error, invalid_move}
    catch 
        error:_Error -> {error, invalid_move}
    end.


make_nested_tuple(Tuple) -> make_nested_tuple(tuple_to_list(Tuple), []).

make_nested_tuple([], Acc) -> list_to_tuple(lists:reverse(Acc));
make_nested_tuple([A,B,C | Rest], Acc) -> make_nested_tuple(Rest, [{A,B,C}| Acc ]);
make_nested_tuple([_ | _], Acc) -> list_to_tuple(lists:reverse(Acc)).
