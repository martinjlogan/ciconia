%% This is the application resource file (.app file) for the erlp,
%% application.
{application, agner_driver,
 [{description, "A driver for an agner based based repo."},
  {vsn, "0.1.0"},
  {modules, [
             agner_driver
            ]},
  {registered,[]},
  {applications, [kernel, stdlib, ewlib, erlware_commons]},
  {start_phases, []}]}.

