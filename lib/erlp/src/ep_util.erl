%%%-------------------------------------------------------------------
%%% @doc Misc functions
%%% @end
%%%-------------------------------------------------------------------
-module(ep_util).

%% API
-export([
	 pack/1,
	 driver/1,
	 repo_type_to_driver/1,
	 join_paths/2
	]).

-include("erlpl.hrl").

%%%===================================================================
%%% API
%%%===================================================================

%% @doc determine which driver to use based on the repo type specified.
-spec driver(option_list()) -> atom().
driver(Options) ->
    repo_type_to_driver(epl_util:get_val(repo_type, Options)).

%% Convert a repo type into a driver name.
-spec repo_type_to_driver(atom()) -> atom().
repo_type_to_driver(undefined) ->
    throw(?UEX(missing_repo_type,
	       "repo type must be defined with the -a option",
	       []));
repo_type_to_driver(Name) ->
    list_to_atom(atom_to_list(Name) ++ "_driver").
    
%% @doc join two paths together
-spec join_paths(string(), string()) -> string().
join_paths(Path, [$/|ChoppedPath2] = Path2) -> 
    case lists:reverse(Path) of
	[$/|_] ->
	    Path ++ ChoppedPath2;
	_ ->
	    Path ++ Path2
    end;
join_paths(Path, Path2) -> 
    case lists:reverse(Path) of
	[$/|_] ->
	    Path ++ Path2;
	_ ->
	    Path ++ "/" ++ Path2
    end.
    
    
%% @doc Tars up a package and returns a binary of the archive.
%% @end
-spec pack(FilePath::string()) -> Archive::binary().
pack(FilePath) ->
    {PackageName, _PackageVsn} = epl_otp_metadata_lib:package_dir_to_name_and_vsn(FilePath),
    TarDirName = filename:basename(FilePath),
    TmpDir = ewl_file:make_tmp_dir(),
    try ok = epl_file:copy(FilePath, filename:join(TmpDir, TarDirName),
			  [recursive])
    catch _:_ -> throw(?EX({copy_failed, FilePath, TmpDir})) end,
    TarName = PackageName ++ ".tar.gz",

    %% Add the tar file name to the end of each path suffix and the repo to the beginning. 
    ?DEBUG("Creating ~s from ~s~n", [TarName, TarDirName]),

    ok            = ewl_file:compress(TarName, [TarDirName], [compressed, {cd, TmpDir}]),
    TarFile       = epl_file:read(filename:join(TmpDir, TarName)),
    ok            = epl_file:remove(TmpDir, [recursive]),
    TarFile.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%%===================================================================
%%% Internal functions
%%%===================================================================

-ifndef(NOTEST).
-include_lib("eunit/include/eunit.hrl").

join_paths_test() ->
    ?assertMatch("a/b", join_paths("a/", "/b")),
    ?assertMatch("a/b", join_paths("a/", "b")),
    ?assertMatch("a/b/", join_paths("a", "b/")),
    ?assertMatch("a/b", join_paths("a", "b")).
    
-endif.
