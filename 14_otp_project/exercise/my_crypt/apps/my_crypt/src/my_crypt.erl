-module(my_crypt).

-behavior(gen_server).

-export([start_link/0, encode/1, get_key/0, set_key/1, hash/1, encode2/2, get_hash_table/0, hash/2, hash2/3]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("records.hrl").


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [],[]).


encode(Data) -> 
    gen_server:call(?MODULE, {encode, Data}).


get_key() -> 
    gen_server:call(?MODULE, get_key).


set_key(Key) -> 
    gen_server:cast(?MODULE, {set_key, Key}),
    ok.


hash(Data) -> 
    gen_server:call(?MODULE, {hash, Data}).


init([]) -> 
    {ok, Key} = application:get_env(my_crypt, crypt_key),
    {ok, Hash_seed} = application:get_env(my_crypt, hash_seed),
    <<A:32, B:32, C:32>> = Hash_seed,
    rand:seed(exsp, {A, B, C}),
    Table = get_hash_table(),
    {ok, #crypt{crypt_key = Key, table = Table}}.


handle_call({encode, Data}, _From, State) ->
    Data2 = binary_to_list(Data),
    Key = binary_to_list(State#crypt.crypt_key),
    Reply = list_to_binary(encode2(Data2, Key)),
    {reply, Reply, State};


handle_call(get_key, _From, State) ->
    Reply = State#crypt.crypt_key,
    {reply, Reply, State};


handle_call({hash, Data}, _From, #crypt{table = Table} = State) ->
    {reply, hash(Data, Table), State};


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



encode2(Data, Key) -> encode2(Data, Key, [], Key).


encode2([], _, List, _Key) ->
    lists:reverse(List);

encode2(Data, [], List, Key) ->
    encode2(Data, Key, List, Key);

encode2([DataHead | DataRest], [KeyHead | KeyRest], List, Key) ->
    encode2(DataRest, KeyRest, [DataHead bxor KeyHead | List], Key).


get_hash_table() ->
    List = lists:seq(1, 256),
    [rand:uniform(Num) || Num <- List].


hash(Message, HashTable) ->
    Data = binary_to_list(Message),
    {ok, Hash_size} = application:get_env(my_crypt, hash_size),
    unicode:characters_to_binary(
    [hash2(Num, Data, HashTable) || Num <- lists:seq(1, Hash_size)]
    ).


hash2(Num, Data, HashTable) ->
    Fun = fun(Char, Hash) -> 
        Index = Char bxor Hash,
        lists:nth(Index + 1, HashTable) 
        end,
    lists:foldl(Fun, Num, Data).
