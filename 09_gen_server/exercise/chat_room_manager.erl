-module(chat_room_manager).

-export([start/0,
         create_room/2, remove_room/2, get_rooms/1,
         add_user/3, remove_user/3, get_users_list/2,
         send_message/4,  get_messages_history/2, stop/1,
         call/3, loop/1, handle_call/3]).


-record(room, {
    id,
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


handle_call(create_room, RoomName, State) -> ok;
handle_call(remove_room, RoomId, State) -> ok;
handle_call(get_rooms, _, State) -> ok;
handle_call(add_user, {RoomId, UserName}, State) -> ok;
handle_call(remove_user, {RoomId, UserName}, State) -> ok;
handle_call(get_users_list, RoomId, State) -> ok;
handle_call(send_message, {RoomId, UserName, Message}, State) -> ok;
handle_call(get_messages_history, RoomId, State) -> ok.
