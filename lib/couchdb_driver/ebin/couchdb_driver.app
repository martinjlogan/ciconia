%% This is the application resource file (.app file) for the erlp,
%% application.
{application, couchdb_driver,
 [{description, "A driver for a couchdb based based repo."},
  {vsn, "0.1.0"},
  {modules, [
             couchdb_driver
            ]},
  {registered,[]},
  %% XXX TODO move the erlp.hrl file so that this can depend on erlp not erlpl
  {applications, [kernel, stdlib, ewlib, erlpl, erlware_commons, couchbeam]},
  {start_phases, []}]}.

