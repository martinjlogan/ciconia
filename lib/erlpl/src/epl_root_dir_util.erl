%%%-------------------------------------------------------------------
%%% @author Martin Logan <mlogan@MLOGAN-MBP>
%%% @copyright (C) 2010, Martin Logan
%%% @doc
%%%  Functions for dealing with and navigating locally installed
%%%  erlang dirs
%%% @end
%%% Created : 26 Aug 2010 by Martin Logan <mlogan@MLOGAN-MBP>
%%%-------------------------------------------------------------------
-module(epl_root_dir_util).

%% API
-export([
	 list_app_vsns/2,
	 list_release_vsns/2,
	 list_apps/1,
	 list_erts/1,
	 list_releases/1,
	 list_app_names/1,
	 list_release_names/1,
	 find_config_file_path/3,
	 find_highest_release_vsn/2,
	 list_vsns/2
	]).

-include("erlpl.hrl").

%%%===================================================================
%%% API
%%%===================================================================

%% @doc return a list of versions installed for a particular application.
%% @spec list_app_vsns(RootDir, AppName) -> [string()]
list_app_vsns(RootDir, AppName) -> 
    list_vsns(list_apps(RootDir), AppName).

%% @doc return a list of versions installed for a particular release.
%% @spec list_release_vsns(RootDir, RelName) -> [string()]
list_release_vsns(RootDir, RelName) -> 
    list_vsns(list_releases(RootDir), RelName).

%% @doc return a list of release names that are currently installed.
%% @spec list_app_names(RootDir) -> [string()]
list_app_names(RootDir) -> 
    list_names(list_apps(RootDir)).

%% @doc return a list of release names that are currently installed.
%% @spec list_release_names(RootDir) -> [string()]
list_release_names(RootDir) -> 
    list_names(list_releases(RootDir)).

%% @doc return a list of applications that are currently installed.
%% @spec list_apps(RootDir) -> [string()]
list_apps(RootDir) -> 
    Fun = fun(PackageDir) -> epl_validation:app_validation(PackageDir) end,
    list_packages(epl_installed_paths:lib_dir(RootDir), Fun).

%% @doc return a list of releases that are currently installed.
%% @spec list_releases(RootDir) -> [string()]
list_releases(RootDir) -> 
    Fun = fun(PackageDir) -> epl_validation:release_validation(PackageDir) end,
    list_packages(epl_installed_paths:releases_dir(RootDir), Fun).

%% @doc return a list of erts packages that are currently installed.
%% @spec list_erts(RootDir) -> [string()]
list_erts(RootDir) -> 
    Fun = fun(PackageDir) -> epl_validation:erts_validation(PackageDir) end,
    list_packages(RootDir, Fun).

%% @doc Return the path to a config file within an installed release.
%% @spec find_config_file_path(RootDir, RelName, RelVsn) -> [ConfigFilePath]
find_config_file_path(RootDir, RelName, RelVsn) -> 
    RelDirPath = ewl_installed_paths:release_file_container_path(RootDir, RelName, RelVsn),
    ewl_file:find(RelDirPath, ".*config$").

%% @doc Find the highest version of a particular release for a particular erts vsn that is installed locally.
%% @spec (ReleaseName, RootDir) -> HighestVsn
find_highest_release_vsn(RelName, RootDir) ->
    case list_release_vsns(RootDir, RelName) of
	[] ->
	    Msg = "The release ~p is not currently installed on the local system~n",
	    throw(?UEX({release_not_found, RelName}, Msg, [RelName]));
	Vsns ->
	    epl_util:highest_vsn(Vsns)
    end.

%% @doc list all the versions of the packages supplied
list_vsns(Packages, BaseName) ->
    lists:foldl(fun(PackageName, Acc) -> 
			case catch epl_otp_metadata_lib:package_dir_to_name_and_vsn(PackageName) of
			    {BaseName, Vsn} -> [Vsn|Acc];
			    _               -> Acc
			end
		end, [], Packages).

%%%===================================================================
%%% Internal functions
%%%===================================================================

list_names(Packages) ->
    Names = lists:foldl(fun(PackageDir, Acc) -> 
				case catch epl_otp_metadata_lib:package_dir_to_name_and_vsn(PackageDir) of
				    {Name, _} -> [Name|Acc];
				    _         -> Acc
				end
			end, [], Packages),
    ordsets:to_list(ordsets:from_list(Names)).

list_packages(Dir, ValidationFun) -> 
    Packages = filelib:wildcard(lists:flatten([Dir, "/*"])),
    Names = lists:foldl(fun(PackageDir, Acc) -> 
				   case ValidationFun(PackageDir) of
				       true ->
					   [PackageDir|Acc];
				       _Error ->
					   Acc
				   end
		     end, [], Packages),
    ordsets:to_list(ordsets:from_list(Names)).

