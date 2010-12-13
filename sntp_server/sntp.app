{application, sntp,
 [{description, "sntp service"},
  {vsn, "0.01"},
  {modules, [
    sntp,
    sntp_app,
    sntp_sup,
    sntp_svc
  ]},
  {registered, []},
  {mod, {sntp_app, []}},
  {env, []},
  {applications, [kernel, stdlib]}]}.

