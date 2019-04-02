-module(short_link).

-export([init/0, create_short/2, get_long/2, rand_str/1, get_root_link/1]).

%%% module API

init() ->
    %% init randomizer
    <<A:32, B:32, C:32>> = crypto:strong_rand_bytes(12),
    rand:seed(exsp, {A,B,C}),
    State = maps:new(),
    State.


get_root_link(Link) ->
    Trim1 = string:nth_lexeme(Link, 1, "."), % "https://hexlet"
    Trim2 = string:nth_lexeme(string:nth_lexeme(Link, 2, "."), 1, "/"), % io
    Trim1 ++ "." ++ Trim2 ++ "/". % https://hexlet.io/



create_short(LongLink, State) ->
    Link_key = get_root_link(LongLink),

    case maps:find(Link_key, State) of 
        {ok, {_Long, Short}} -> {Short, State};
        error -> 
            Short = Link_key ++ rand_str(8), % https://hexlet.io/qZZQofRC
            {Short, maps:put(Link_key, {LongLink, Short}, State)}
    end.


get_long(ShortLink, State) ->
    Link_key = get_root_link(ShortLink),

    case maps:find(Link_key, State) of 
        {ok, {Long, _Short}} -> {ok, Long};
        error -> {error, not_found}
    end.


%% generates random string of chars [a-zA-Z0-9]
rand_str(Length) ->
    lists:map(fun(Char) when Char > 83 -> Char + 13;
                 (Char) when Char > 57 -> Char + 7;
                 (Char) -> Char
              end,
              [rand:uniform(110 - 48) + 47 || _ <- lists:seq(1, Length)]).
