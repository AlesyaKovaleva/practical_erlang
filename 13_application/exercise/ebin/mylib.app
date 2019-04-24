{
    application,
    mylib,
    [
        {description, "My Lib"},
        {vsn, "0.1"},
        {registered, []},
        {mod, {mylib_app, []}},
        {applications, [kernel, stdlib]},
        {env,[
            {min_val, 2},
            {max_val, 10},
            {connection_timeout, 10000},
            {query_timeout, 10000}
        ]},
        {modules, [mylib_app, mylib_sup, mylib_worker]}
    ]
}.
