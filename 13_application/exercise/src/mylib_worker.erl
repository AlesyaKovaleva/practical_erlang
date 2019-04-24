-module(mylib_worker).
-behavior(gen_server).

-export([start_link/0, get_version/0, get_modules/0, get_min_val/0, get_connection_timeout/0, all_apps/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [],[]).


get_version() -> 
    gen_server:call(?MODULE, get_version).


get_modules() -> 
    gen_server:call(?MODULE, get_modules).


get_min_val() -> 
    gen_server:call(?MODULE, get_min_val).


get_connection_timeout() -> 
    gen_server:call(?MODULE, get_connection_timeout).


all_apps() -> 
    gen_server:call(?MODULE, all_apps).


init([]) -> 
    {ok, #{}}.


handle_call(get_version, _From, State) ->
    {ok, Reply} = application:get_key(mylib, vsn),
    {reply, Reply, State};


handle_call(get_modules, _From, State) ->
    {ok, Reply} = application:get_key(mylib, modules),
    {reply, Reply, State};


handle_call(get_min_val, _From, State) ->
    {ok, Reply} = application:get_env(mylib, min_val),
    {reply, Reply, State};


handle_call(get_connection_timeout, _From, State) ->
    {ok, Reply} = application:get_env(mylib, connection_timeout),
    {reply, Reply, State};


handle_call(all_apps, _From, State) ->
    Reply = lists:foldl(
        fun({Application, Description, Vsn}, Map) ->
            Map#{Application => #{description => Description, version => Vsn}}
        end, 
    #{},
    application:which_applications()
    ),
    
    {reply, Reply, State};


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