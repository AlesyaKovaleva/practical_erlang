-module(sup_2).

-behaviour(supervisor).

-export([start_link/0, init/1, add_worker/1, remove_worker/1, make_worker/1]).


start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).


init([]) ->
    SupervisorSpecification = #{
        strategy => one_for_one,
        intensity => 10,
        period => 1000},

    ChildSpecifications =
        [
        make_worker(3),
        make_worker(4)
        ],
    {ok, {SupervisorSpecification, ChildSpecifications}}.


add_worker(Id) -> 
    supervisor:start_child(?MODULE, make_worker(Id)).


remove_worker(Id) -> 
    supervisor:terminate_child(?MODULE, Id),
    supervisor:delete_child(?MODULE, Id).


make_worker(Id) ->
    #{id => Id,
           start => {worker, start_link, [Id]},
           restart => permanent,
           shutdown => 2000,
           type => worker,
           modules => [worker]}.
