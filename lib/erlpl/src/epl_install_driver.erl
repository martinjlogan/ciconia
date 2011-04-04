%%%-------------------------------------------------------------------
%%% @author Martin Logan <martinjlogan@Macintosh.local>
%%% @copyright (C) 2010, Martin Logan
%%% @doc
%%%  Contains policy free install functions. 
%%% @end
%%% Created : 11 Jun 2010 by Martin Logan <martinjlogan@Macintosh.local>
%%%-------------------------------------------------------------------
-module(epl_install_driver).

%% API
-export([
	 install_app/2,
	 install_release/6,
	 install_script_and_boot/3,
	 install_config_file/3,
	 install_erts/2,
	 install_erts/3,
	 install_executables/2
	]).

-include("erlpl.hrl").

%%%===================================================================
%%% API
%%%===================================================================

%% @doc install an application. Pass this function any directory
%% containing a valid OTP application and it will install it in the
%% lib dir of the RootDir passed in.
-spec install_app(AppDir::string(), RootDir::string()) -> AppDir::string().
install_app(AppDir, RootDir) ->
    {AppName, AppVsn} = epl_otp_metadata_lib:app_name_and_vsn(AppDir),
    InstalledAppDir   = epl_installed_paths:app_dir(RootDir, AppName, AppVsn),
    clean_copy_dir(AppDir, InstalledAppDir),
    InstalledAppDir.

%% @doc create a release from the various release artifacts.
%%      Target systems are always structured as follows.
%% <pre>
%%  BaseDir>/bin/[executable_files]
%%  BaseDir>/releases/DotRelFile [config_file]
%%  BaseDir>/releases/[erts_dir]
%%  BaseDir>/lib/[applications]
%%  BaseDir>/[optional_dirs]
%% </pre>
%% @spec (RelFile, ConfigFile, ExecutableFile, LibDirs, ErtsDir, RootDir) -> ok
%% where
%%  RelDir = string()
%%  ConfigFile = string() | undefined
%%  ExecutableFiles = [string()] | undefined
%%  LibDirs = [string()] | string()
%%  ErtsDir = string() | undefined
%%  RootDir = string() | undefined
install_release(RelDir, ConfigFile, ExecutableFiles, LibDirs, ErtsDir, RootDir) ->
    RelFile   = epl_util:fetch_rel_file_from_release_package(RelDir),
    EbinPaths = install_apps(RelFile, LibDirs, RootDir),
    install_script_and_boot(RootDir, RelFile, EbinPaths),
    install_config_file(RootDir, RelFile, ConfigFile),
    install_erts(RelFile, RootDir, ErtsDir),
    install_executables(RootDir, ExecutableFiles).

%% @doc Install a list of executable files
%% @spec (RootDir, Executables) -> ok
%% where
%%  Executables = [string()]
install_executables(_RootDir, undefined) ->
    ok;
install_executables(RootDir, Executables) ->
    BinDir = epl_installed_paths:bin_dir(RootDir),
    try ewl_file:mkdir_p(BinDir)
    catch _:_ -> throw(?EX({mkdir_failed, BinDir})) end,
    lists:foreach(
      fun(File) ->
	      InstalledFile = filename:join(BinDir, filename:basename(File)),  
	      clean_copy_file(File, InstalledFile),
	      epl_os_specific:set_executable(InstalledFile)
      end,
      Executables).

%% @doc Install erts
%% @spec (RelFile, RootDir, ErtsDir) -> ok
%% XXX TODO remove this - this is extra contract checking at the driver
%% layer that probably can be moved to the policy layer.
install_erts(_RelFile, _RootDir, undefined) ->
    ok;
install_erts(RelFile, RootDir, ErtsDir) ->
    ErtsVsn = epl_otp_metadata_lib:consult_rel_file(erts_vsn, RelFile),
    InstalledErtsDir2 = filename:join(RootDir, filename:basename(ErtsDir)), 
    InstalledErtsDir  = epl_installed_paths:erts_dir(RootDir, ErtsVsn),
    ok = compare_erts_vsns(InstalledErtsDir, InstalledErtsDir2),
    install_erts(ErtsDir, RootDir).

-spec install_erts(string(), string()) -> ok.
install_erts(ErtsDir, RootDir) ->
    ?DEBUG("installing erts from ~s to ~s~n", [ErtsDir, RootDir]),
    {"erts", ErtsVsn} =
	epl_otp_metadata_lib:package_dir_to_name_and_vsn(ErtsDir),
    InstalledErtsDir  = epl_installed_paths:erts_dir(RootDir, ErtsVsn),
    try
	case filelib:is_dir(InstalledErtsDir) of
	    true ->
		?DEBUG("ERTS already present at ~s no need to overwrite~n",
		       [InstalledErtsDir]),
		ok;
	    false ->
		ok = clean_copy_dir(ErtsDir, InstalledErtsDir),
		epl_os_specific:set_all_executable(
		  filename:join([InstalledErtsDir, "bin"]))
	end
    catch
	_:_ -> throw(?EX({failed_to_install_erts, ErtsDir, InstalledErtsDir}))
    end.

compare_erts_vsns(InstalledErtsDir, InstalledErtsDir2) ->
    compare_erts_vsns2(filename:basename(InstalledErtsDir),
		       filename:basename(InstalledErtsDir2)).

compare_erts_vsns2(InstalledErtsDir, InstalledErtsDir) ->
    ok;
compare_erts_vsns2(InstalledErtsDir, InstalledErtsDir2) ->
    throw(?EX({mismatched_erts_vsns, InstalledErtsDir, InstalledErtsDir2})).
    

%% @doc Install a config file along with the release file it is
%%      associated with.
%% @spec (RootDir, RelFile, ConfigFile) -> ok
install_config_file(_RootDir, _RelFile, undefined) ->
    ok;
install_config_file(RootDir, RelFile, ConfigFile) ->
    InstalledRelDir = installed_rel_dir(RelFile, RootDir),
    InstalledConfigFile = filename:join(InstalledRelDir,
					filename:basename(ConfigFile)),
    try
	ok = clean_copy_file(ConfigFile, InstalledConfigFile)
    catch
	_:_ -> throw(?EX({copy_config_failed, RelFile, InstalledRelDir}))
    end.

%% @doc install script and boot files for a given rel file. EbinPaths
%%      are all the ebin paths for the apps required by the .rel file.
%% @spec (RootDir, RelFile, EbinPaths) -> ok
install_script_and_boot(RootDir, RelFile, EbinPaths) ->
    [RelName, RelVsn] =
	epl_otp_metadata_lib:consult_rel_file([name, vsn], RelFile),
    RelDir = filename:dirname(RelFile),
    InstalledRelDir =
	epl_installed_paths:release_dir(RootDir, RelName, RelVsn),
    InstalledRelFile =
	epl_installed_paths:rel_file_path(RootDir, RelName, RelVsn),
    epl_file:remove(InstalledRelDir, [recursive]),
    clean_copy_dir(RelDir, InstalledRelDir),
    make_script(InstalledRelFile, [local, no_module_tests, {path, EbinPaths}]).

%%%===================================================================
%%% Internal functions
%%%===================================================================
    
make_script(RelFile, Opts) ->
    NoSuffix = string:substr(RelFile, 1, length(RelFile) - 4),
    case systools:make_script(NoSuffix, Opts) of
	ok -> 
	    ok;
	Error -> 
	    ?WARN("ERROR - Make script and boot failed ~p~n", [Error]),
	    exit(?EX({script_and_boot_failed, RelFile, Opts}))
    end.

%% Returns the paths to the ebin dirs of all apps installed
install_apps(RelFile, LibDirs, RootDir) ->
    AppSpecs = epl_otp_metadata_lib:consult_rel_file(app_specs, RelFile),
    [filename:join(
       install_app_from_app_spec(AppSpec,
				 if_string_make_list(LibDirs),
				 RootDir), ebin) ||
	AppSpec <- AppSpecs].
	
%% If lib dirs is only a string make it a list
if_string_make_list([H|_] = LibDir) when is_integer(H) ->
    [LibDir];
if_string_make_list(LibDirs) ->
    LibDirs.
							
install_app_from_app_spec(AppSpec, [LibDir|T], RootDir) ->   
    try
	InstalledAppDir =
	    app_dir(AppSpec, epl_installed_paths:lib_dir(RootDir)),
	case filelib:is_dir(InstalledAppDir) of
	    true ->
		InstalledAppDir;
	    false ->
		AppDir = app_dir(AppSpec, LibDir),
		install_app(AppDir, RootDir)
	end
    catch
	_:_ ->
	    install_app_from_app_spec(AppSpec, T, RootDir)
    end;
install_app_from_app_spec(AppSpec, [], _RootDir) ->   
    {AppName, AppVsn} = extract_app_vsn_from_appspec(AppSpec),
    AppNameAndVsn     = name_and_vsn(AppName, AppVsn),
    throw(?EX({app_not_found, AppNameAndVsn})).
    
app_dir(AppSpec, LibDir) ->   
    {AppName, AppVsn} = extract_app_vsn_from_appspec(AppSpec),
    filename:join([LibDir, name_and_vsn(AppName, AppVsn)]).
	
extract_app_vsn_from_appspec(AppSpec) when is_tuple(AppSpec) ->
    {element(1, AppSpec), element(2, AppSpec)}.

name_and_vsn(AppName, AppVsn) when is_atom(AppName) ->
    name_and_vsn(atom_to_list(AppName), AppVsn);
name_and_vsn(AppName, AppVsn) ->
    AppName ++ "-" ++ AppVsn.
    
installed_rel_dir(RelFile, RootDir) ->
    [RelName, RelVsn] =
	epl_otp_metadata_lib:consult_rel_file([name, vsn], RelFile),
    epl_installed_paths:release_dir(RootDir, RelName, RelVsn).

clean_copy_dir(Dir, Dir) ->
    ?DEBUG("attempting to copy a directory to itself with ~p~n", [Dir]),
    ok;
clean_copy_dir(Dir, DestDir) ->
    epl_file:remove(DestDir, [recursive]),
    ewl_file:copy(Dir, DestDir, [recursive]).

clean_copy_file(File, File) ->
    ?DEBUG("attempting to copy a file to itself with ~p~n", [File]),
    ok;
clean_copy_file(File, DestFile) ->
    epl_file:remove(DestFile),
    epl_file:copy(File, DestFile).
