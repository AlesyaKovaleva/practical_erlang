-module(sup_1).

-behaviour(supervisor).

-export([start_link/0, init/1]).

%% TODO
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).


init([]) ->
    SupervisorSpecification = #{
        strategy => one_for_one,
        intensity => 10,
        period => 1000},

    ChildSpecifications =
        [
        #{id => worker_1,
           start => {worker, start_link, [1]},
           restart => transient,
           shutdown => 2000,
           type => worker,
           modules => [worker]},

        #{id => worker_2,
           start => {worker, start_link, [2]},
           restart => transient,
           shutdown => 2000,
           type => worker,
           modules => [worker]}
        ],
    {ok, {SupervisorSpecification, ChildSpecifications}}.
