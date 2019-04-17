-module(chat_room_manager).
-behavior(gen_server).

-export([start_link/0,
         create_room/1, get_rooms/0,
         add_user/3, remove_user/2, get_users/1,
         send_message/3,  get_history/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).


-record(state, {
    rooms = []
}).


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [],[]).

% **create_room/1** создает новую комнату.
% Принимает имя комнаты (binary) и возвращает комнату в виде кортежа _{Name, Pid}_.
create_room(RoomName) ->
    {ok, PidRoom} = chat_room:start_link(),
    gen_server:cast(?MODULE, {create_room, {RoomName, PidRoom}}),
    {RoomName, PidRoom}.


% **get_rooms/0** возвращает список комнат, где каждая комната представлена
% кортежем _{Name, Pid}_. Комнаты в списке могут идти в произвольном порядке.
get_rooms() ->
    gen_server:call(?MODULE, get_rooms).


% **add_user/3** добавляет нового пользователя в комнату.
% Принимает pid комнаты, имя пользователя (binary) и pid пользователя.
% Возвращает атом _ok_, если выполнилась успешно.
% Или возвращает кортеж _{error, room\_not\_found}_, если заданая комната не найдена.
add_user(PidRoom, UserName, PidUser) ->
    gen_server:call(?MODULE, {add_user, {PidRoom, UserName, PidUser}}).


% **remove_user/2** удаляет пользователя из комнаты.
% Принимает pid комнаты и pid пользователя. Возвращает _ok_, если пользователь
% успешно удален, или _{error, user\_not\_found}_, если такого пользователя
% в комнате не было, или _{error, room\_not\_found}_, если заданая комната не найдена.
remove_user(PidRoom, PidUser) ->
    gen_server:call(?MODULE, {remove_user, {PidRoom, PidUser}}).


% **get_users/1** отдает список пользователей в комнате.
% Принимает pid комнаты. В успешном случае возвращает кортеж _{ok, Users}_,
% Где Users -- список пользователей, представленых кортежем вида _{Name, Pid}_.
% Или возвращает кортеж _{error, room\_not\_found}_, если заданая комната не найдена.
get_users(PidRoom) ->
    gen_server:call(?MODULE, {get_users, PidRoom}).


% **send_message/3** посылает новое сообщение в комнату.
% Принимает pid комнаты, имя автора сообщения (binary)
% и текст сообщения (binary). В случае успеха возвращает атом _ok_.
% Или возвращает кортеж _{error, room\_not\_found}_, если заданая комната не найдена.
send_message(PidRoom, UserName, Message) ->
    gen_server:call(?MODULE, {send_message, {PidRoom, UserName, Message}}).


% **get_history/1** отдает список всех сообщений, пришедших в комнату,
% сортированный от более старых сообщений к более новым.
% Принимает pid комнаты. В случае успеха возвращает кортеж _{ok, Messages}_,
% где Messages -- список сообщений, представленых кортежем _{Name, Text}_.
% Или возвращает кортеж _{error, room\_not\_found}_, если заданая комната не найдена.
get_history(PidRoom) ->
    gen_server:call(?MODULE, {get_history, PidRoom}).


% Не возбраняется реализовать и функцию **close_room/1** и добавить для нее тест :)


init([]) -> 
    {ok, #state{}}.


handle_cast({create_room, {RoomName, PidRoom}}, #state{rooms = Rooms} = State) ->
    NewState = State#state{rooms = [{RoomName, PidRoom} | Rooms]},
    {noreply, NewState}.


handle_call(get_rooms, _From, State) ->
    {reply, State#state.rooms, State};


handle_call({add_user, {PidRoom, UserName, PidUser}}, _From, #state{rooms = Rooms} = State) ->
    Reply = case lists:keymember(PidRoom, 2, Rooms) of 
                false -> {error, room_not_found};
                true -> chat_room:add_user(PidRoom, UserName, PidUser), ok
            end,
    {reply, Reply, State};


handle_call({remove_user, {PidRoom, PidUser}}, _From, #state{rooms = Rooms} = State) ->
    Reply = case lists:keymember(PidRoom, 2, Rooms) of 
                false -> {error, room_not_found};
                true -> chat_room:remove_user(PidRoom, PidUser)
            end,
    {reply, Reply, State};


handle_call({get_users, PidRoom}, _From, #state{rooms = Rooms} = State) ->
    Reply = case lists:keymember(PidRoom, 2, Rooms) of 
                false -> {error, room_not_found};
                true -> {ok, chat_room:get_users(PidRoom)}
            end,
    {reply, Reply, State};


handle_call({send_message, {PidRoom, UserName, Message}}, _From, #state{rooms = Rooms} = State) ->
    Reply = case lists:keymember(PidRoom, 2, Rooms) of 
                false -> {error, room_not_found};
                true -> chat_room:add_message(PidRoom, UserName, Message)
            end,
    {reply, Reply, State};


handle_call({get_history, PidRoom}, _From, #state{rooms = Rooms} = State) ->
    Reply = case lists:keymember(PidRoom, 2, Rooms) of 
                false -> {error, room_not_found};
                true -> {ok, chat_room:get_history(PidRoom)}
            end,
    {reply, Reply, State}.


handle_info(_, State) ->
    {noreply, State}.


terminate(_, _) ->
    ok.


code_change(_, State, _) -> 
    {ok, State}.
