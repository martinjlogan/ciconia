%% This is the application resource file (.app file) for the erlp_local,
%% application.
{application, erlpl, 
  [{description, "Local package management with no remote component"},
   {vsn, "0.5.0"},
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
		epl_installed_info,

		% commands
		epl_help,
		epl_version,
		epl_managed,
		epl_test,
		epl_list,
		epl_manage_root_dir,
		epl_config_file_path,
		epl_install_release,
		epl_rollback_release,
		epl_install_app,
		epl_install_erts,
		epl_remove_app,
		epl_remove_release
              ]},
   {registered,[]},
   {applications, [kernel, stdlib, getopt, ewrepo, erlware_commons, ewlib]},
   {start_phases, []}]}.

