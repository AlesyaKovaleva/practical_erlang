-module(mcache_storage).

-behavior(gen_server).

-export([start_link/0, set/2, get/1, delete/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).


%%% module API

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


set(Key, Val) ->
    gen_server:call(?MODULE, {set, Key, Val}).

get(Key) ->
    gen_server:call(?MODULE, {get, Key}).


delete(Key) ->
    gen_server:call(?MODULE, {delete, Key}).

%%% gen_server API

init([]) ->
    Table = ets:new(?MODULE, [named_table]),
    {ok, Table}.


handle_call({set, Key, Val}, _From, State) ->
    ets:insert(State, {Key, Val}),
    {reply, ok, State};


handle_call({get, Key}, _From, State) ->
    Reply = case ets:lookup(State, Key) of 
                [] -> {error, not_found};
                [{Key, Val}] -> {ok, Val}
            end,
    {reply, Reply, State};


handle_call({delete, Key}, _From, State) ->
    ets:delete(State, Key),
    {reply, ok, State};


handle_call(Request, _From, State) ->
    lager:warning("Unknown call ~p in ~p~n", [Request, ?MODULE]),
    {noreply, State}.


handle_cast(Request, State) ->
    lager:warning("Unknown cast ~p in ~p~n", [Request, ?MODULE]),
    {noreply, State}.


handle_info(Request, State) ->
    lager:warning("Unknown info ~p in ~p~n", [Request, ?MODULE]),
    {noreply, State}.


terminate(_Reason, _State) ->
    ok.


code_change(_OldVersion, State, _Extra) ->
    {ok, State}.