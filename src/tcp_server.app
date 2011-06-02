{application, tcp_server,
 [
  {description, "Demo TCP server"},
  {vsn, "1.0"},
  {id, "tcp_server"},
  {modules,      [tcp_listener, tcp_user_fsm, map_server]},
  {registered,   [tcp_server_sup, tcp_listener, map_server]},
  {applications, [kernel, stdlib]},
  %%
  %% mod: Specify the module name to start the application, plus args
  %%
  {mod, {tcp_server_app, []}},
  {env, [{file, "server.log"}]}
 ]
}.
