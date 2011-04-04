%% This is the application resource file (.app file) for the erlp,
%% application.
{application, erlp,
 [{description, "Remote package management. Handles pulling and publishing remote packages."},
  {vsn, "0.3.0"},
  {modules, [
	     ep_util,
	     ep_help,
             ep_search,
             ep_publish,
             ep_update_cache,
             ep_install_app,
             ep_install_erts,
             ep_install_release,
             ep_install_util,
             ep_cache
            ]},
  {registered,[]},
  {applications, [kernel, stdlib, getopt, erlpl, ewlib, erlware_commons]},
  {start_phases, []}]}.

