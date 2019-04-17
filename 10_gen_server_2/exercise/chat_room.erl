-module(chat_room).
-behavior(gen_server).

-export([start_link/0, add_user/3, remove_user/2, get_users/1, add_message/3, get_history/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
    users = [],
    history = []
}).


start_link() ->
    gen_server:start_link(?MODULE, [],[]).


add_user(PidRoom, UserName, PidUser) ->
    gen_server:cast(PidRoom, {add_user, {UserName, PidUser}}), ok.


remove_user(PidRoom, PidUser) ->
    gen_server:call(PidRoom, {remove_user, PidUser}).


get_users(PidRoom) ->
    gen_server:call(PidRoom, get_users).


add_message(PidRoom, UserName, Message) ->
    gen_server:cast(PidRoom, {add_message, {UserName, Message}}), ok.


get_history(PidRoom) ->
    gen_server:call(PidRoom, get_history).


init([]) ->
    {ok, #state{}}.


handle_call({remove_user, PidUser}, _From, #state{users = Users} = State) -> 
    case lists:keymember(PidUser, 2, Users) of 
        false -> {reply, {error, user_not_found}, State};
        true -> NewUsers = lists:keydelete(PidUser, 2, Users),
                {reply, ok, State#state{users = NewUsers}}
    end;


handle_call(get_users, _From, #state{users = Users} = State) ->
    {reply, Users, State};


handle_call(get_history, _From, #state{history = History} = State) -> 
        {reply, lists:reverse(History), State}.


handle_cast({add_user, {UserName, PidUser}}, #state{users = Users} = State) -> 
    NewState = State#state{users = [{UserName, PidUser} | Users]},
    {noreply, NewState};


handle_cast({add_message, {UserName, Message}}, #state{users = Users, history = History} = State) -> 
    NewState = State#state{history = [{UserName, Message} | History]},
    Fun = fun(PidUser) -> chat_user:add_message(PidUser, UserName, Message) end,
    lists:keymap(Fun, 2, Users),
    {noreply, NewState}.


handle_info(_, State) ->
    {noreply, State}.


terminate(_, _) ->
    ok.


code_change(_, State, _) -> 
    {ok, State}.
