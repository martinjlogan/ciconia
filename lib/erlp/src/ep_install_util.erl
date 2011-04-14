%%%-------------------------------------------------------------------
%%% @doc
%%%  Commands common to install app and install release.
%%% @end
%%%-------------------------------------------------------------------
-module(ep_install_util).

%% API
-export([
	 ep_cache_fetch_ensure/2,
	 fetch_binary/2,
	 write_out_package/2
	]).


-include("erlpl.hrl").

%%%===================================================================
%%% API
%%%===================================================================
%% @doc Just for convenience. Throw an exception if there are no records.
-spec ep_cache_fetch_ensure(string(), list()) -> list().
ep_cache_fetch_ensure(PackageName, []) ->
    throw(?UEX(no_package_cache, "Can't install ~p:~n" ++
	       "Package cache is either unreadable or not updated~n" ++
	       "Try using 'update_cache -r <repo_url>'.~n" ++
	       "If that doesn't work check the permissions on your" ++
	       " cache file.",
	       [PackageName]));
ep_cache_fetch_ensure(_PackageName, List) ->
    List.

%% @doc fetches a binary package from the repo
-spec fetch_binary(PackageInfo::tuple(), [tuple()]) -> binary().
fetch_binary(PackageInfo, Options) ->
    Timeout = epl_util:get_option(timeout, Options),
    Driver = ep_util:repo_type_to_driver(PackageInfo#package_info.repo_type),
    ?DEBUG("using the ~p module for a driver~n", [Driver]),
    {ok, Got} = Driver:get(PackageInfo, Timeout),
    Got.

%% @doc write a binary package out to disk in a temporary location.
-spec write_out_package(PackageInfo::tuple(), PackageBinary::binary()) -> FilePath::string().
write_out_package(#package_info{name = Name, vsn = Vsn} = P, PackageBinary)
  when is_binary(Name), is_binary(Vsn) ->
    write_out_package(P#package_info{name = binary_to_list(Name), vsn = binary_to_list(Vsn)}, PackageBinary); 
write_out_package(#package_info{name = Name, vsn = Vsn}, PackageBinary) ->
    TmpDir = ewl_file:make_tmp_dir(),
    %% XXX TODO this can't always be assumed to be .tar.gz
    FilePath = ewl_file:join_paths(TmpDir, Name ++ "-" ++ Vsn ++ ".tar.gz"),
    ?DEBUG("writing binary to ~s~n", [FilePath]),
    write_data(PackageBinary, FilePath),
    FilePath.

%%%======================================================================
%%% Internal functions
%%%======================================================================

write_data(Data, To) ->
    try
	{ok, Fd} = file:open(To, [write, raw]),
	ok = file:write(Fd, Data),
	file:close(Fd),
	ok
    catch
        {error, Reason} ->
            throw(?UEX({file_open_error, Reason},
		       "Could not write to ~p~nCheck your file system permissions~n",
		       [To]))
    end.

