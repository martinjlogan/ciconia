%%%-------------------------------------------------------------------
%%% @author Martin Logan <martinjlogan@erlware.org>
%%% @doc Understands the path structure of various packages types and states.  In order to keep that information encapsulated
%%% here there are a few path dependent operations in this module as well. 
%%% @copyright (C) 2007, Martin Logan, Erlware
%%% @end
%%%-------------------------------------------------------------------
-module(epl_installed_paths).

%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------
-include("eunit.hrl").

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([
	 releases_dir/1,
	 lib_dir/1,
	 bin_dir/1,
	 erts_dir/2,
	 release_dir/3,
	 app_dir/3,
	 release_bin_dir/3
	]).

-export([
	 rel_file_path/3,
	 app_file_path/3,
	 config_file_path/4
	]).

-export([
	 managed_root_dir_file_path/1,
	 package_info_path/1
	]).

%%====================================================================
%% Fundamental Paths
%%====================================================================

%% @doc Returns the path to the file containing all managed root
%%      dir paths.
%% @spec managed_root_dir_file_path(MetaDir) -> string()
managed_root_dir_file_path(MetaDir) ->
    filename:join(MetaDir, "managed_root_dirs.erlpl").

%% @doc Returns the path to the directory releases are stored in.
%% @spec releases_dir(RootDir) -> string()
releases_dir(RootDir) ->
    filename:join([RootDir, "releases"]).

%% @doc Returns the path to the directory applications are stored in.
%% @spec lib_dir(RootDir) -> string()
lib_dir(RootDir) ->
    filename:join([RootDir, "lib"]).

%% @doc Returns a path to the directory where executable files sit. 
%% @spec bin_dir(RootDir) -> string()
bin_dir(RootDir) -> 
    filename:join([RootDir, "bin"]).

%% @doc Returns a path to the directory under which all the erts packages lie.
%% @spec erts_dir(RootDir, ErtsVsn) -> string()
erts_dir(RootDir, ErtsVsn) -> 
    filename:join([RootDir, "erts-" ++ ErtsVsn]).

%% @doc Returns a path to an installed release.
%% @spec release_dir(RootDir, RelName, RelVsn) -> string()
release_dir(RootDir, RelName, RelVsn) ->
    filename:join([releases_dir(RootDir), RelName ++ "-" ++ RelVsn]).

%% @doc returns a path to an installed application.
%% @spec app_dir(RootDir, RelName, RelVsn) -> string()
app_dir(RootDir, AppName, AppVsn) ->
    filename:join([lib_dir(RootDir), AppName ++ "-" ++ AppVsn]).

%% @doc Returns the full path to a rel file.
%% @spec rel_file_path(RootDir, RelName, RelVsn) -> string()
rel_file_path(RootDir, RelName, RelVsn) -> 
    filename:join([release_dir(RootDir, RelName, RelVsn), RelName ++ ".rel"]).

%% @doc Returns the full path to an app file.
%% @spec app_file_path(RootDir, AppName, AppVsn) -> string()
app_file_path(RootDir, AppName, AppVsn) -> 
    filename:join([app_dir(RootDir, AppName, AppVsn), "ebin", AppName ++ ".app"]).

%% @doc Returns the path to the bin directory in an installed release.
%% @spec release_bin_dir(RootDir, RelName, RelVsn) -> string()
release_bin_dir(RootDir, RelName, RelVsn) -> 
    filename:join([release_dir(RootDir, RelName, RelVsn), "bin"]).

%% @doc return the path to config. 
%% @spec config_file_path(RootDir, RelName, RelVsn, ConfigFileName) -> string()
config_file_path(RootDir, RelName, RelVsn, ConfigFileName) ->
    filename:join([release_dir(RootDir, RelName, RelVsn), ConfigFileName]).

%% @doc path to the package info file for a given package.
%% @spec package_info_path(PackageDir) -> string().
package_info_path(PackageDir) ->
    filename:join(PackageDir, "pkg.info").

%%%--------------------------------------------------------------------
%%% Tests
%%%--------------------------------------------------------------------

releases_dir_test() ->
    ?assertMatch("/foo/releases", releases_dir("/foo")).

lib_dir_test() ->
    ?assertMatch("/foo/lib", lib_dir("/foo")).

bin_dir_test() ->
    ?assertMatch("/foo/bin", bin_dir("/foo")).

erts_dir_test() ->
    ?assertMatch("/foo/erts-5.7.4", erts_dir("/foo", "5.7.4")).

release_dir_test() ->
    ?assertMatch("/foo/releases/rel-5.7.4", release_dir("/foo", "rel", "5.7.4")).

app_dir_test() ->
    ?assertMatch("/foo/lib/app-5.7.4", app_dir("/foo", "app", "5.7.4")).

release_bin_dir_test() ->
    ?assertMatch("/foo/releases/rel-5.7.4/bin", release_bin_dir("/foo", "rel", "5.7.4")).

rel_file_path_test() ->
    ?assertMatch("/foo/releases/rel-5.7.4/rel.rel", rel_file_path("/foo", "rel", "5.7.4")).

app_file_path_test() ->
    ?assertMatch("/foo/lib/app-5.7.4/ebin/app.app", app_file_path("/foo", "app", "5.7.4")).

config_file_path_test() ->
    ?assertMatch("/foo/releases/rel-5.7.4/sys.config", config_file_path("/foo", "rel", "5.7.4", "sys.config")).
