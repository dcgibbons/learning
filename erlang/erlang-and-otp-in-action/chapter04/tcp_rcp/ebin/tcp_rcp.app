{application, tcp_rcp,
    [{description, "RPC server for Erlang and OTP in auction"},
     {vsn, "0.1.0"},
     {modules, [tr_app, tr_sup, tr_server]},
     {registered, [tr_sup]},
     {applications, [kernel, stdlib]},
     {mod, {tr_app, []}}
]}.
