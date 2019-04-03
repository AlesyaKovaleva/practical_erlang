-module(group_by_solution).

-export([
    get_users/0, 
    group_by_gender1/1, 
    group_by_age/1, 
    group_by_main/2, 
    group_by_detail_age/1, 
    group_by_gender2/1,
    get_sessions/0,
    group_session_by_node/1,
    group_session_by_type/1
]).


get_users() ->
    [
        {user, "Bob", 12, male},
        {user, "Bill", 12, male},
        {user, "Helen", 25, female},
        {user, "Kate", 25, female},
        {user, "John", 61, male}
    ].


get_sessions() ->
    [
        {session, type_a, node_1, 1},
        {session, type_b, node_1, 2},
        {session, type_a, node_2, 3},
        {session, type_b, node_2, 4}
    ].


% First task
group_by_gender1(Users) -> 
    Male = [ User || {user, _Name, _Age, Gender} = User <- Users, Gender == male],
    Female = [ User || {user, _Name, _Age, Gender} = User <- Users, Gender == female],

    #{male => Male, female => Female}.


% Second task
group_by_main(Fun, List) ->
    Values = [Fun(User) || User <- List],
    lists:foldl(
        fun({Criteria, User}, Map) ->
            case maps:find(Criteria, Map) of 
                {ok, Value} -> maps:put(Criteria, [User | Value], Map);
                error -> maps:put(Criteria, [User], Map)
            end
        end,
        #{},
        Values
    ).


group_by_age(User) -> 
    {user, _Name, Age, _Gender} = User,
    if 
        Age >= 0, Age =< 12 -> {child, User};
        Age > 12, Age =< 18 -> {teenager, User};
        Age > 18, Age =< 25 -> {young, User};
        Age > 25, Age =< 60 -> {adult, User};
        Age > 60 -> {old, User};
        true -> {undefined, User}
    end.


group_by_detail_age(User) ->
    {user, _Name, Age, _Gender} = User,
    {Age, User}.


group_by_gender2(User) ->
    {user, _Name, _Age, Gender} = User,
    {Gender, User}.


group_session_by_node(Session) ->
    {session, _Type, Node, _SocketId} = Session,
    {Node, Session}.


group_session_by_type(Session) ->
    {session, Type, _Node, _SocketId} = Session,
    {Type, Session}.
