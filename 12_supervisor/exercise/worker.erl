-module(worker).
-behavior(gen_server).

-export([start_link/1, ping/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
    id :: term()
}).


start_link(Id) ->
    gen_server:start_link(?MODULE, [Id],[]).


ping(Pid) ->
    gen_server:call(Pid, ping).


init([Id]) -> 
    {ok, #state{id = Id}}.


handle_call(ping, _From, State) ->
    {reply, {State#state.id, self()}, State};


handle_call(_, _From, State) ->
    {noreply, State}.


handle_cast(_, State) ->
    {noreply, State}.


handle_info(_, State) ->
    {noreply, State}.


terminate(_, _) ->
    ok.


code_change(_, State, _) -> 
    {ok, State}.