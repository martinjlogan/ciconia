%% This is the application resource file (.app file) for the erlp,
%% application.
{application, erlp,
 [{description, "Remote package management. Handles pulling and publishing remote packages."},
  {vsn, "0.3.0"},
  {modules, [
	     ep_util,
	     ep_cmd_help,
             ep_cmd_search,
             ep_cmd_publish,
             ep_cmd_update_cache,
             ep_cmd_install_app,
             ep_cmd_install_erts,
             ep_cmd_install_release,
             ep_install_util,
             ep_cache
            ]},
  {registered,[]},
  {applications, [kernel, stdlib, getopt, erlpl, ewlib, erlware_commons]},
  {start_phases, []}]}.

