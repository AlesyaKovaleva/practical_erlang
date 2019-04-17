-module(chat_user).
-behavior(gen_server).
% behaviour(gen_server) требует, чтобы наш модуль определил функции 
% init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2 и code_change/3.

-export([start_link/0, add_message/3, get_messages/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
        messages = []
         }).

start_link() ->
    gen_server:start_link(?MODULE, [], []).  

% chat\_room и chat\_user запускаются в нескольких экземплярах,
% так что их регистрировать не нужно, а нужно обращаться к ним по pid
% потока.


init([]) ->
    {ok, #state{}}.

% Аргумент init, это данные, которые мы передавали третьим аргументом в gen_server:start_link. 
% Здесь нужно создать структуру данных, которая будет хранить состояние сервера.


add_message(UserPid, UserName, Message) ->
    gen_server:cast(UserPid, {add_messages, {UserName, Message}}), ok.


get_messages(UserPid) -> 
    gen_server:call(UserPid, get_messages).
% Вызов gen_server:call блокирует клиента, пока сервер не обработает его запрос и не вернет ответ. 


handle_cast({add_messages, Data}, #state{messages = Messages} = State) ->
    NewState = State#state{messages = [ Data | Messages]},
    {noreply, NewState}.
% когда клиенту ответ сервера не нужен. Тогда лучше использовать gen_server:cast. 
% Клиент не блокируется и не ждет ответ сервера. Но сервер получает и обрабатывает сообщение.


handle_call(get_messages, _From, #state{messages = Messages} = State) -> 
    Reply = lists:reverse(Messages),
    {reply, Reply, State}.


handle_info(_, State) ->
    {noreply, State}.
% Любой поток из любого места в коде может отправить серверу сообщение оператором !. 
% Так делать не рекомендуется, потому что это вызовы в обход API сервера. Но иногда так делают.
% Если сообщения в функции loop сервера приходят не из gen_server:call/cast, 
% то они обрабатываются в callback-функции handle_info.
% Запрос и State


terminate(_, _) ->
    ok.
% Этот callback вызывается, когда gen_server останавливается. 
% Если поток в процессе своей работы занимал какие-то ресурсы 
% (соединение с базой данных, сокеты, файлы и т.д.), 
% то по правилам OTP предлагается освобождать их здесь.
% Причина и State


code_change(_, State, _) -> 
    {ok, State}.
% принимает на входе старый #state{}, и должен его преобразовать и вернуть новый #state{}.