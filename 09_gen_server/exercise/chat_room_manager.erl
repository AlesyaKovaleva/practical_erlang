-module(chat_room_manager).

-export([start/0,
         create_room/2, remove_room/2, get_rooms/1,
         add_user/3, remove_user/3, get_users_list/2,
         send_message/4,  get_messages_history/2, stop/1,
         call/3, loop/1, handle_call/3]).


-define(CountRooms, 5).


-record(room, {
    name,
    users = [],
    messages = []
}).


start() ->
    InitialState = #{},
    spawn(?MODULE, loop, [InitialState]).


create_room(Server, RoomName) ->
    call(Server, create_room, RoomName).


remove_room(Server, RoomId) ->
    call(Server, remove_room, RoomId).  


get_rooms(Server) ->
    call(Server, get_rooms, none).


add_user(Server, RoomId, UserName) ->
    call(Server, add_user, {RoomId, UserName}).


remove_user(Server, RoomId, UserName) ->
    call(Server, remove_user, {RoomId, UserName}).


get_users_list(Server, RoomId) ->
    call(Server, get_users_list, RoomId).


send_message(Server, RoomId, UserName, Message) ->
    call(Server, send_message, {RoomId, UserName, Message}).


get_messages_history(Server, RoomId) ->
    call(Server, get_messages_history, RoomId).


call(Server, Command, Payload) -> 
    QueryRef = erlang:monitor(process, Server),
    Server ! {Command, QueryRef, self(), Payload},
    receive 
        {reply, QueryRef, Reply} -> 
            erlang:demonitor(QueryRef, [flush]),
            Reply;
        {'DOWN', QueryRef, _process, _Server, Reason} -> 
            % io:format("Server crached - Reason: ~p~n", [Reason]),
            {error, Reason}
    after 
        5000 -> 
            erlang:demonitor(QueryRef, [flush]),
            {error, no_reply}
    end.        


stop(Server) ->
    Server ! stop.


loop(State) -> 
    % io:format("V3 Loop ~p Current State: ~p~n", [self(), State]),
    receive
        {Command, QueryRef, ClientPid, Payload} ->
            {Reply, State2} = ?MODULE:handle_call(Command, Payload, State),
            ClientPid ! {reply, QueryRef, Reply},
            ?MODULE:loop(State2);
        stop -> ok;
        _Msg -> ?MODULE:loop(State)
    end.


handle_call(create_room, RoomName, State) -> 
    case maps:size(State) of 
        Num when Num >= ?CountRooms -> {{error, room_limit}, State};
        _Num -> RoomId = make_ref(), Room = #room{name = RoomName},
            State2 = State#{RoomId => Room},
                {{ok, RoomId}, State2}
    end;


handle_call(remove_room, RoomId, State) -> 
    case maps:find(RoomId, State) of 
        {ok, _Value} -> State2 = maps:remove(RoomId, State), {ok, State2};
        error -> {{error, room_not_found}, State}
    end;

% [{RoomId, RoomName}]
handle_call(get_rooms, _, State) -> 
    Fun = fun(RoomId, #room{name = RoomName}, Acc) ->
        [{RoomId, RoomName} | Acc] end,
    Rooms = maps:fold(Fun, [], State),
    {Rooms, State};


handle_call(add_user, {RoomId, UserName}, State) -> 
    case maps:find(RoomId, State) of 
        {ok, #room{users = UserList} = Ulist} -> 
            case lists:member(UserName, UserList) of 
                true -> {{error, user_is_in_room}, State};
                false -> Ulist2 = Ulist#room{users = [UserName | UserList]},
                         State2 = State#{RoomId => Ulist2}, {ok, State2}         
            end;
        error -> {{error, room_not_found}, State}
    end;


handle_call(remove_user, {RoomId, UserName}, State) -> 
    case maps:find(RoomId, State) of 
        {ok, #room{users = UserList} = Ulist} -> 
            case lists:member(UserName, UserList) of 
                true -> Ulist2 = Ulist#room{users = lists:delete(UserName, UserList)},
                        State2 = State#{RoomId => Ulist2}, {ok, State2};
                false -> {{error, user_not_in_room}, State}         
            end;
        error -> {{error, room_not_found}, State}
    end;


handle_call(get_users_list, RoomId, State) -> 
    case maps:find(RoomId, State) of 
        {ok, #room{users = UserList}} -> 
            {{ok, UserList}, State};
        error -> {{error, room_not_found}, State}
    end;


handle_call(send_message, {RoomId, UserName, Message}, State) -> 
    case maps:find(RoomId, State) of 
        {ok, #room{users = UserList, messages = Messages} = Ulist} -> 
            case lists:member(UserName, UserList) of 
                true -> Ulist2 = Ulist#room{messages = [{UserName, Message} | Messages]}, 
                        State2 = State#{RoomId => Ulist2}, {ok, State2};
                false -> {{error, user_not_in_room}, State}        
            end;
        error -> {{error, room_not_found}, State}
    end;


handle_call(get_messages_history, RoomId, State) ->
    case maps:find(RoomId, State) of 
        {ok, #room{messages = Messages}} -> {{ok, Messages}, State};
        error -> {{error, room_not_found}, State}
    end.