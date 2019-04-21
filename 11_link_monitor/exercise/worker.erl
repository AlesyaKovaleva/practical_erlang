-module(worker).

-export([get_data/2]).


get_data(File, PidParent) -> 
    {ok, Data} = file:read_file(File),
    Data2 = binary:split(Data, [<<"\n">>], [global]),
    Data3 = lists:filter(fun(Bin) -> Bin /= <<>> end, Data2),
    DataMap = lists:foldl(
        fun(A, Map) -> 
            List = binary:split(A, [<<",">>], [global]),
            Key = lists:nth(2, List),
            Value = list_to_integer(binary_to_list(lists:nth(3, List))),
            case maps:find(Key, Map) of 
                {ok, Val} -> Map#{Key := Val+Value};
                error -> Map#{Key => Value}
            end
        end,
        #{},
        Data3
    ),

    PidParent ! {data, DataMap},
    ok.
