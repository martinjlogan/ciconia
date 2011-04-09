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
		epl_cmd_help,
		epl_cmd_version,
		epl_cmd_managed,
		epl_cmd_test,
		epl_cmd_list,
		epl_cmd_manage_root_dir,
		epl_cmd_config_file_path,
		epl_cmd_install_release,
		epl_cmd_rollback_release,
		epl_cmd_install_app,
		epl_cmd_install_erts,
		epl_cmd_remove_app,
		epl_cmd_remove_release
              ]},
   {registered,[]},
   {applications, [kernel, stdlib, getopt, ewrepo, erlware_commons, ewlib]},
   {start_phases, []}]}.

