-module(main_sup).

-behaviour(supervisor).

-export([start_link/0, init/1]).


start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).


init([]) ->
    SupervisorSpecification = #{
        strategy => one_for_one,
        intensity => 10,
        period => 1000},

    ChildSpecifications =
        [
        #{id => child_sup_1,
           start => {sup_1, start_link, []},
           restart => transient,
           shutdown => 2000,
           type => supervisor,
           modules => [sup_1]},

        #{id => child_sup_2,
           start => {sup_2, start_link, []},
           restart => transient,
           shutdown => 2000,
           type => supervisor,
           modules => [sup_2]}
        ],
    {ok, {SupervisorSpecification, ChildSpecifications}}.
