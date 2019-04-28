-module(url_parser).

-export([
    parse/1, protocol_verify/1, domain_verify/1, 
    query_verify/1, path_verify/1, data_verify/1, 
    get_data/2, get_data2/1
]).


-spec parse(binary()) -> {ok, map()} | {error, term()}.
parse(URL) ->
    try 
        {Protocol, DomainRest} = protocol_verify(URL),
        {Domain, QueryRest} = domain_verify(DomainRest),
        {Path, Query} = query_verify(QueryRest),
        Path2 = path_verify(Path),
        Date = data_verify(Path),

        DataMap = #{
            protocol => Protocol, 
            domain => Domain, 
            path => Path2, 
            query => Query, 
            date => Date
        },
        
        {ok, DataMap}
    catch 
        throw:Error -> Error 
    end.


protocol_verify(URL) ->
    case binary:split(URL, [<<"://">>]) of 
        [Protocol, _Rest] -> {Protocol, _Rest};
        [_InvalidProtocol] -> throw({error, invalid_protocol})
    end.


domain_verify(URL2) ->
    case binary:split(URL2, [<<"/">>]) of 
        [<<>>] -> throw({error, invalid_domain});
        [Domain, Rest] -> {Domain, Rest}
    end.    


query_verify(URL3) ->
    case binary:split(URL3, [<<"?">>]) of 
        [Path] -> {Path, <<>>};
        [Path2, Query] -> {Path2, Query}
    end.


path_verify(URL4) ->
    Path = binary:split(URL4, [<<"/">>], [global]),
    lists:delete(<<>>, Path). 


data_verify(Path) ->
    Path2 = string:split(binary_to_list(Path), "/", all),
    case Path2 of 
        [Y, M, D | _] -> get_data([Y, M, D], []);
        _ -> undefined
    end.



get_data([], List) -> get_data2(lists:reverse(List));
get_data([H | D], List) -> 
    case string:to_integer(H) of 
        {Num, []} -> get_data(D, [Num | List]);
        {error, no_integer} -> undefined 
    end.


get_data2([Y, M, D]) when M >= 1, M =< 12, D >= 1, D =< 31 -> {Y, M, D};
get_data2(_) -> undefined.
