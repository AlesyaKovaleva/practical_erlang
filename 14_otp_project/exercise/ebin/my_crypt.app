{
    application,
    my_crypt,
    [
        {description, "My Crypt"},
        {vsn, "0.1"},
        {registered, []},
        {mod, {my_crypt_app, []}},
        {applications, [kernel, stdlib]},
        {env,[]},
        {modules, [my_crypt_app, my_crypt_sup, my_crypt]}
    ]
}.
