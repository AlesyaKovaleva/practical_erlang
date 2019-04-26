-module(my_crypt).

-behavior(gen_server).

-export([start_link/0, encode/1, get_key/0, set_key/1, hash/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("records.hrl").


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [],[]).

% **my_crypt:encode/1** -- принимает на вход исходные данные (binary) и
% возвращает зашифрованные данные (binary).
encode(Data) -> 
    gen_server:call(?MODULE, {encode, Data}).

% **my_crypt:get_key/0** -- возвращает текущее значение ключа шифрования (binary).
get_key() -> 
    gen_server:call(?MODULE, get_key).

% **my_crypt:set_key/1** -- задает новое значение ключа шифрования (binary).
set_key(Key) -> 
    gen_server:cast(?MODULE, {set_key, Key}),
    ok.

% **my_crypt:hash/1**, которая принимает на вход
% исходные данные (binary) и возвращает хэш для них (binary).
hash(Data) -> 
    gen_server:call(?MODULE, {hash, Data}).


init([]) -> 
    {ok, Key} = application:get_env(my_crypt, crypt_key),
    {ok, Hash_size} = application:get_env(my_crypt, hash_size),

    {ok, #crypt{crypt_key = Key, hash_size = Hash_size}}.


handle_call({encode, Data}, _From, State) ->
    {reply, ok, State};


handle_call(get_key, _From, State) ->
    Reply = State#crypt.crypt_key,
    {reply, Reply, State};


handle_call({hash, Data}, _From, State) ->
    {reply, ok, State};


handle_call(_, _From, State) ->
    {noreply, State}.


handle_cast({set_key, NewKey}, State) ->
    State2 = State#crypt{crypt_key = NewKey},
    {noreply, State2};


handle_cast(_, State) ->
    {noreply, State}.


handle_info(_, State) ->
    {noreply, State}.


terminate(_, _) ->
    ok.


code_change(_, State, _) -> 
    {ok, State}.