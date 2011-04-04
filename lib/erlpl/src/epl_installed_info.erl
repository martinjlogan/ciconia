%%%-------------------------------------------------------------------
%%% @author Martin Logan <martinjlogan@Macintosh.local>
%%% @copyright (C) 2011, Martin Logan
%%% @doc
%%%  Information about all installed packages under management.
%%% @end
%%% Created : 19 Mar 2011 by Martin Logan <martinjlogan@Macintosh.local>
%%%-------------------------------------------------------------------
-module(epl_installed_info).

%% API
-export([
	 managed_root_dirs/1,
	 add_managed_root_dir/1,
	 write/2,
	 write_to/3,
	 all_apps/1,
	 all_releases/1,
	 all_erts/1,
	 erts_under_root/1,
	 erts_under_root/2,
	 releases_under_root/2,
	 release_under_root/3,
	 apps_under_root/2,
	 app_under_root/3,
	 erts/1,
	 erts/2,
	 releases/2,
	 release/3,
	 apps/2,
	 app/3
	]).

-include("erlpl.hrl").

%%%===================================================================
%%% API
%%%===================================================================
%% @doc Fetch all apps under management
-spec all_apps(option_list()) -> list().
all_apps(Options) ->
    %% XXX TODO perhaps a list form right away for speed
    lists:foldl(fun(ContainerDir, Acc) ->
			fetch_package_info(ContainerDir, Acc)
		end, [], lib_dirs(Options)).

%% @doc Fetch all releases under management
-spec all_releases(option_list()) -> list().
all_releases(Options) ->
    lists:foldl(fun(ContainerDir, Acc) ->
				     fetch_package_info(ContainerDir, Acc)
			     end, [], releases_dirs(Options)).

%% @doc Fetch all erts under management
-spec all_erts(option_list()) -> list().
all_erts(Options) ->
    lists:foldl(fun(ContainerDir, Acc) ->
			fetch_package_info(ContainerDir, Acc)
		end, [], managed_root_dirs(Options)).

%% @doc add a managed root dir to the list of root dirs
-spec add_managed_root_dir(option_list()) -> ok.
add_managed_root_dir(Options) ->
    MetaDir = epl_util:get_val(meta_dir, Options),
    assert_not_undefined(MetaDir, meta_dir),
    ewl_file:mkdir_p(MetaDir),
    RootDir = epl_util:get_val(root_dir, Options),
    assert_not_undefined(RootDir, root_dir),
    MRootDirFile = epl_installed_paths:managed_root_dir_file_path(MetaDir),
    RootDirList = consult_root_dirs(MRootDirFile),
    AbsRootDir = filename:absname(RootDir),
    epl_file:write_term(MRootDirFile,
			[AbsRootDir|lists:delete(AbsRootDir, RootDirList)]).

assert_not_undefined(undefined, Option) ->
    throw(?EX({option_must_be_set, Option}));
assert_not_undefined(_Value, _Option) ->
    ok.

%% @doc add a managed root dir to the list of root dirs
-spec managed_root_dirs(option_list()) -> ok.
managed_root_dirs(Options) ->
    MetaDir = epl_util:get_val(meta_dir, Options),
    MRootDirFile = epl_installed_paths:managed_root_dir_file_path(MetaDir),
    consult_root_dirs(MRootDirFile).

%% @doc Fetch a specific release, under the root dir specified in Options
-spec releases_under_root(string(), option_list()) -> [tuple()].
releases_under_root(Name, Options) ->
    RootDir = epl_util:get_val(root_dir, Options),
    ContainerDir = epl_installed_paths:releases_dir(RootDir),
    fetch_package_info_by_name(Name, ContainerDir).

%% @doc Fetch a specific release, under the root dir specified in Options
-spec release_under_root(string(), string(), option_list()) -> tuple().
release_under_root(Name, Vsn, Options) ->
    RootDir = epl_util:get_val(root_dir, Options),
    ContainerDir = epl_installed_paths:releases_dir(RootDir),
    fetch_head(fetch_package_info_by_name_and_vsn(Name, Vsn, ContainerDir)).

%% @doc Fetch a release give a name under any root dir.
-spec releases(string(), option_list()) -> [tuple()].
releases(Name, Options) ->
    fetch_package_info_by_name(Name, releases_dirs(Options)).
    
%% @doc Fetch a release give a name and version under any root dir.
-spec release(string(), string(), option_list()) -> tuple().
release(Name, Vsn, Options) ->
    fetch_head(fetch_package_info_by_name_and_vsn(Name, Vsn,
						  releases_dirs(Options))).
    
%% @doc Fetch a specific app, under the root dir specified in Options
-spec apps_under_root(string(), option_list()) -> [tuple()].
apps_under_root(Name, Options) ->
    RootDir = epl_util:get_val(root_dir, Options),
    ContainerDir = epl_installed_paths:lib_dir(RootDir),
    fetch_package_info_by_name(Name, ContainerDir).

%% @doc Fetch a specific app, under the root dir specified in Options
-spec app_under_root(string(), string(), option_list()) -> tuple().
app_under_root(Name, Vsn, Options) ->
    RootDir = epl_util:get_val(root_dir, Options),
    ContainerDir = epl_installed_paths:lib_dir(RootDir),
    fetch_head(fetch_package_info_by_name_and_vsn(Name, Vsn, ContainerDir)).

%% @doc Fetch an app give a name under any root dir.
-spec apps(string(), option_list()) -> [tuple()].
apps(Name, Options) ->
    fetch_package_info_by_name(Name, lib_dirs(Options)).
    
%% @doc Fetch an app give a name and version under any root dir.
-spec app(string(), string(), option_list()) -> tuple().
app(Name, Vsn, Options) ->
    fetch_head(fetch_package_info_by_name_and_vsn(Name, Vsn,
						  lib_dirs(Options))).
    
%% @doc Fetch a specific erts, under the root dir specified in Options
-spec erts_under_root(option_list()) -> [tuple()].
erts_under_root(Options) ->
    RootDir = epl_util:get_val(root_dir, Options),
    fetch_package_info_by_name("erts", RootDir).

%% @doc Fetch a specific erts, under the root dir specified in Options
-spec erts_under_root(string(), option_list()) -> tuple().
erts_under_root(Vsn, Options) ->
    RootDir = epl_util:get_val(root_dir, Options),
    fetch_head(fetch_package_info_by_name_and_vsn("erts", Vsn, RootDir)).

%% @doc Fetch an erts give a name under any root dir.
-spec erts(option_list()) -> [tuple()].
erts(Options) ->
    fetch_package_info_by_name("erts", managed_root_dirs(Options)).

%% @doc Fetch an erts given a name and version under any root dir.
-spec erts(string(), option_list()) -> tuple().
erts(Vsn, Options) ->
    fetch_head(fetch_package_info_by_name_and_vsn("erts", Vsn,
						  managed_root_dirs(Options))).

%% Write information about a package into a local package.
-spec write(string(), option_list()) -> ok.
write(PackageDir, Options) ->
    write_to(PackageDir,
	     epl_installed_paths:package_info_path(PackageDir),
	     Options).

%% Write information about a package into the specified location.
-spec write_to(string(), string(), option_list()) -> ok.
write_to(PackageDir, To, Options) ->
    PackageInfo = epl_otp_metadata_lib:package_info(PackageDir, Options),
    epl_file:write_term(To, PackageInfo).
    
%%%===================================================================
%%% Internal functions
%%%===================================================================
fetch_head(List) ->
    try
	hd(List)
    catch
	_C:_E ->
	    throw(?UEX(package_not_found,
		       "The package you are trying to find locally is not~n" ++
		       "present. Try installing it first",
		       []))
    end.

consult_root_dirs(MRootDirFile) ->
    try epl_file:consult(MRootDirFile) of
	RootDirList ->
	    RootDirList
    catch
	_C:_E ->
	    []
    end.

lib_dirs(Options) ->
    package_dirs(Options,
		 fun(RootDir_) -> epl_installed_paths:lib_dir(RootDir_) end).

releases_dirs(Options) ->
    package_dirs(Options,
		 fun(RootDir_) -> epl_installed_paths:releases_dir(RootDir_) end).

package_dirs(Options, DirPathFun) ->
    lists:foldl(fun(RootDir_, Acc) ->
			[DirPathFun(RootDir_)|Acc]
		end,
		[], managed_root_dirs(Options)).
    

fetch_package_info(ContainerDir, Dict) ->
    Packages = filelib:wildcard(lists:flatten([ContainerDir, "/*"])),
    lists:foldl(fun(PackageDir, Acc) -> 
			PkgInfo = epl_installed_paths:package_info_path(PackageDir),
			try epl_file:consult(PkgInfo) of
			    Record ->
				% epl_util:dictafy(Record, Acc)
				[Record|Acc]
			catch
			    _C:_E ->
				Acc
			end
		end, Dict, Packages).

fetch_package_info_by_name(Name, [H|_] = ContainerDir) when is_integer(H) ->
    fetch_package_info_by_name_and_vsn(Name, any, ContainerDir);
fetch_package_info_by_name(Name, [ContainerDir|T]) ->
    lists:concat([fetch_package_info_by_name(Name, ContainerDir),
		  fetch_package_info_by_name(Name, T)]);
fetch_package_info_by_name(_Name, []) ->
    [].

fetch_package_info_by_name_and_vsn(Name, Vsn, [H|_] = ContainerDir) when is_integer(H) ->
    Packages = filelib:wildcard(lists:flatten([ContainerDir, "/*"])),
    lists:foldl(
      fun(PackageDir, Acc) ->
	      try epl_otp_metadata_lib:package_dir_to_name_and_vsn(PackageDir) of
		  {Name, Vsn} ->
		      acc_pkg_info(PackageDir, Acc);
		  {Name, _Vsn} when Vsn == any ->
		      acc_pkg_info(PackageDir, Acc);
		  {Name, _Vsn} ->
		      Acc;
		  {_Name, _Vsn} ->
		      Acc
	      catch
		  _:_ ->
		      Acc
	      end
      end, [], Packages);
fetch_package_info_by_name_and_vsn(Name, Vsn, [ContainerDir|T]) ->
    lists:concat([fetch_package_info_by_name_and_vsn(Name, Vsn, ContainerDir),
		  fetch_package_info_by_name_and_vsn(Name, Vsn, T)]);
fetch_package_info_by_name_and_vsn(_Name, _Vsn, []) ->
    [].
    
acc_pkg_info(PackageDir, List) ->
    PkgInfoPath = epl_installed_paths:package_info_path(PackageDir),
    try  
	[epl_file:consult(PkgInfoPath)|List]
    catch 
	_:_ -> List
    end.

%%%===================================================================
%%% Testing Functions
%%%===================================================================

-ifndef(NOTEST).
-include_lib("eunit/include/eunit.hrl").

-endif.
