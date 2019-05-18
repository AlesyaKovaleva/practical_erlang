-module(mcache_server).

-behavior(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3, hello/0]).
-export([accept/2, handle_connection/3, parse/1, storage/2]).


%%% module API

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

hello() ->
    42.

%%% gen_server API

init([]) ->
    {ok, Port} = application:get_env(mcache, port),
    {ok, Connections_count} = application:get_env(mcache, connections_count),
    {ok, ListenSocket} = gen_tcp:listen(Port, [binary, {active, false}, {packet, line}, {reuseaddr, true}]),
    lager:info("Start server at port ~p with ~p connections~n", [Port, Connections_count]),
    [spawn(?MODULE, accept, [Id, ListenSocket]) || Id <- lists:seq(1, Connections_count)],
    {ok, #{}}.


handle_call(_Request, _From, State) ->
    {noreply, State}.


handle_cast(_Request, State) ->
    {noreply, State}.


handle_info(_Request, State) ->
    {noreply, State}.


terminate(_Reason, _State) ->
    ok.


code_change(_OldVersion, State, _Extra) ->
    {ok, State}.


accept(Id, ListenSocket) ->
    lager:info("Socket #~p wait for client~n", [Id]),
    case gen_tcp:accept(ListenSocket) of 
        {ok, AcceptSocket} -> 
            lager:info("Socket #~p, session started~n", [Id]),
            handle_connection(Id, ListenSocket, AcceptSocket);
        {error, Error} -> 
            lager:error("Socket #~p can't be open because of ~p~n", [Id, Error])
    end.


handle_connection(Id, ListenSocket, AcceptSocket) ->
    case gen_tcp:recv(AcceptSocket, 0) of
        {ok, Msg} -> 
            lager:info("Socket #~p got message: ~p~n", [Id, Msg]),
            Data = parse(Msg),
            Reply = <<Data/binary, "\r\n">>,
            gen_tcp:send(AcceptSocket, Reply),
            handle_connection(Id, ListenSocket, AcceptSocket);
        {error, Reason} ->
            lager:error("!Socket #~p, session ~p~n", [Id, Reason]),
            accept(Id, ListenSocket)
    end.


parse(Msg) ->
    Data = binary:split(binary:replace(Msg, <<"\r\n">>, <<"">>), <<" ">>),
    case Data of 
        [<<"SET">>, Rest] -> storage(set, binary:split(Rest, <<" ">>));
        [<<"GET">>, Key] -> storage(get, Key);
        [<<"GETS">>, Keys] -> storage(gets, binary:split(Keys, <<" ">>, [global]));
        [<<"DELETE">>, Key] -> storage(delete, Key);
        [<<"ADD">>, Rest] -> storage(add, binary:split(Rest, <<" ">>));
        [<<"REPLACE">>, Rest] -> storage(replace, binary:split(Rest, <<" ">>));
        [<<"APPEND">>, Rest] -> storage(append, binary:split(Rest, <<" ">>));
        [<<"PREPEND">>, Rest] -> storage(prepend, binary:split(Rest, <<" ">>));
        _ -> <<"UNKNOWN REQUEST">>
    end.


storage(set, [Key, Value | _Rest]) -> 
    mcache_storage:set(Key, Value),
    <<"STORED">>;

storage(get, Key) -> 
    case mcache_storage:get(Key) of 
        {error, not_found} -> <<"NOT FOUND">>;
        {ok, Value} -> <<"VALUE ", Key/binary, " ", Value/binary, "\r\nEND">>
    end;

storage(gets, Keys) -> 
    Fun = fun(Key) ->
            case mcache_storage:get(Key) of 
                {error, not_found} -> <<"VALUE ", Key/binary, " ", "NOT FOUND", "\r\n">>;
                {ok, Value} -> <<"VALUE ", Key/binary, " ", Value/binary, "\r\n">>
            end
        end,
    Reply = [lists:map(Fun, Keys) | <<"END">>],
    list_to_binary(Reply);
    
storage(delete, Key) -> 
    case mcache_storage:get(Key) of 
        {error, not_found} -> <<"NOT FOUND">>;
        {ok, _Value} -> mcache_storage:delete(Key), <<"DELETED">>
    end;

storage(add, [Key, Value | _Rest]) -> 
    case mcache_storage:get(Key) of 
        {error, not_found} -> mcache_storage:set(Key, Value), <<"STORED">>;
        {ok, _Value} -> <<"EXISTS">>
    end;   

storage(replace, [Key, Value | _Rest]) -> 
    case mcache_storage:get(Key) of 
        {error, not_found} -> <<"NOT FOUND">>;
        {ok, _Value} -> mcache_storage:set(Key, Value), <<"STORED">>
    end; 

storage(append, [Key, Value | _Rest]) -> 
    case mcache_storage:get(Key) of 
        {error, not_found} -> <<"NOT FOUND">>;
        {ok, Val} -> 
            NewVal = <<Val/binary, Value/binary>>,
            mcache_storage:set(Key, NewVal), <<"STORED">>
    end;

storage(prepend, [Key, Value | _Rest]) -> 
    case mcache_storage:get(Key) of 
        {error, not_found} -> <<"NOT FOUND">>;
        {ok, Val} -> 
            NewVal = <<Value/binary, Val/binary>>,
            mcache_storage:set(Key, NewVal), <<"STORED">>
    end.
