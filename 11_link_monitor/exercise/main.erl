-module(main).

-export([parse/1, processing/2, merger/2, pidref_to_map/1]).

-define(WORKER, worker).


parse(Files) ->
    PidRef = [{File, spawn_monitor(?WORKER, get_data, [File, self()])} || File <- Files],
    PidRef2 = pidref_to_map(PidRef),
    processing(PidRef2, {#{}, #{}}).


processing(PidRef, Acc) when map_size(PidRef) == 0 -> Acc;
processing(PidRef, {DataMap, ErrorMap}) ->
    receive 
        {data, Data} -> 
            DataMap2 = merger(Data, DataMap),
            processing(PidRef, {DataMap2, ErrorMap});

        {'DOWN', Ref, process, Pid, Reason} -> 
            PidRef2 = maps:remove({Pid, Ref}, PidRef),
            case Reason of 
                normal -> processing(PidRef2, {DataMap, ErrorMap});
                _ -> File = maps:get({Pid, Ref}, PidRef), 
                    ErrorMap2 = ErrorMap#{File => Reason},
                    processing(PidRef2, {DataMap, ErrorMap2})
            end

    after 2000 -> timeout
    end.


merger(Map1, Map2) -> 
    maps:fold(
    fun(Name, Count, Acc) ->
        case maps:find(Name, Map2) of
            {ok, Count2} -> Acc#{Name => Count+Count2};
            error -> Acc#{Name => Count}
        end
    end,
    Map2,
    Map1).


pidref_to_map(PidRef) ->
    lists:foldl(
        fun({File, PR}, Acc) ->
            Acc#{PR => File}
        end, 
    #{},
    PidRef
    ).
