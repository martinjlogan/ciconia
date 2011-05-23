%% This is the application resource file (.app file) for the erlp,
%% application.
{application, erlp,
 [{description, "Remote package management. Handles pulling and publishing remote packages."},
  {vsn, "0.5.0"},
  {modules, [
	     ep_util,
             ep_install_util,
             ep_cache
            ]},
  {registered,[]},
  {applications, [kernel, stdlib, getopt, erlpl, ewlib, erlware_commons, agner_driver]},
  {start_phases, []}]}.

