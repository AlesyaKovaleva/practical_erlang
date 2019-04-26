-module(my_crypt_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).


start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).


init([]) ->
    SupervisorSpecification = #{
        strategy => one_for_one,
        intensity => 10,
        period => 1000},

    ChildSpecifications =
        [
        #{id => my_crypt,
           start => {my_crypt, start_link, []},
           restart => permanent,
           shutdown => 2000,
           type => worker,
           modules => [my_crypt]}
        ],
    {ok, {SupervisorSpecification, ChildSpecifications}}.
