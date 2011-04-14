%% This is the application resource file (.app file) for the erlp_local,
%% application.
{application, erlpl, 
  [{description, "Local package management with no remote component"},
   {vsn, "0.7.0"},
   {modules, [
		epl_cmdln_lib,
		epl_otp_metadata_lib,
		epl_validation,
		epl_util,
		epl_file,
		epl_os_specific,
	    	epl_root_dir_util,
	    	epl_install_driver,
		epl_installed_paths,
		epl_installed_info
              ]},
   {registered,[]},
   {applications, [kernel, stdlib, getopt, ewrepo, erlware_commons, ewlib]},
   {start_phases, []}]}.

