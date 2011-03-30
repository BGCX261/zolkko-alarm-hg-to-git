{application, smokehouse,
 [{description, "smokehouse"},
  {vsn, "0.01"},
  {modules, [
    smokehouse,
    smokehouse_app,
    smokehouse_sup,
    smokehouse_web,
    smokehouse_deps
  ]},
  {registered, []},
  {mod, {smokehouse_app, []}},
  {env, [
    {db_service, "mnesia_service"}
  ]},
  {applications, [kernel, stdlib, crypto]}]}.
