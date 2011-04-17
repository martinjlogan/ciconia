%%%-------------------------------------------------------------------
%%% @author Martin Logan <martinjlogan@Macintosh.local>
%%% @copyright (C) 2011, Erlware
%%% @doc a driver for dealing with a web dav faxien style repo.
%%% @end
%%%-------------------------------------------------------------------
-module(faxien_driver).

%% API
-export([
	 put/4,
	 list/2,
	 get/2
	]).

-include("erlpl.hrl").

%%%===================================================================
%%% API
%%%===================================================================

%% @doc this function is used to list out all data in a repo based
%%      on a search criteria provided as a package info record.
%%      This returns a list of package info records.
-spec list(record() | list(), non_neg_integer()) -> list().
list([H|_] = RepoURL, Timeout) when is_integer(H) ->
    ?INFO("Pulling cache data from ~p~n" ++
	  "** This may take up to a few minutes **~n",
	  [RepoURL]), 
    get_package_infos(RepoURL, Timeout);
list(Repos, Timeout) when is_list(Repos) ->
    % XXX TODO changed to lists map for exception propagation.
    % This can change when pmap handles exceptions correctly.
    % this temp hack destroys timeouts.
    lists:foldl(fun(RepoURL, Acc) ->
                      list(RepoURL, Timeout) ++ Acc
              end, [], Repos).

%% @doc this function is used to get a package from the repo
-spec get(record(), non_neg_integer()) -> binary().
get(PackageInfo, Timeout) ->
    PackageURL = PackageInfo#package_info.path,
    try
	Resp = {ok, _Payload} = fd_repo_dav:repo_get(PackageURL, Timeout),
	Resp
    catch
        _C:E ->
            throw(?UEX({repo_connect_failure, {E, PackageURL}},
                       "The repo specified by ~p~ncould not be contacted. " ++
                       "Check your net connection~nor try your search " ++
                       "without the offending repo.",
                       [PackageURL]))
    end.

%%--------------------------------------------------------------------
%% @doc place a binary into the repo.
%%  "erts" | "release" | "app--source" | "app--binary" | "app--binary".
%% @end
%%--------------------------------------------------------------------
-spec put(list(), string(), record(), non_neg_integer()) -> list().
put(Repos, PackageDir, PackageInfo, Timeout) ->
    PackageBin = ep_util:pack(PackageDir),

    #package_info{executable_env = ExecutableEnv,
		  package_type = PackageType,
		  name = Name,
		  vsn = Vsn} = PackageInfo,

    % list seems to be good, now time to replace package context with no record
    % just a function call that publishes an app given some info

    [$e,$r,$t,$s,$-|ErtsVsn] = ExecutableEnv,

    publish2(PackageType, Repos, ErtsVsn, Name, Vsn,
	     PackageBin, PackageInfo, Timeout).
    
    
%%====================================================================
%% Internal functions
%%====================================================================

%%% List functions

get_package_infos(RepoURL, Timeout) ->
    ErtsVsns = fd_repo_dav_repo_list(RepoURL, Timeout),
    OSTypeURLs = build_os_type_urls(RepoURL, ErtsVsns, Timeout),
    RawPackageInfoList = get_all_package_info(OSTypeURLs, Timeout),
    format_package_info_list(RawPackageInfoList, RepoURL).

format_package_info_list(RawPackageInfoList, RepoURL) ->
    lists:map(fun(RawPackageInfo) -> format_package_info(RawPackageInfo, RepoURL) end, RawPackageInfoList).
		      
format_package_info(#package_info{package_type = erts} = P, RepoURL) ->
    P#package_info{repo = RepoURL, repo_type = faxien, package_type = "erts"};
format_package_info(#package_info{package_type = release} = P, RepoURL) ->
    P#package_info{repo = RepoURL, repo_type = faxien,
		   package_type = "release"};
format_package_info(#package_info{package_type = app_binary_specific} = P,
		    RepoURL) ->
    P#package_info{repo = RepoURL, repo_type = faxien,
		   package_type = "app--binary"};
format_package_info(#package_info{package_type = app_binary_generic} = P,
		    RepoURL) ->
    P#package_info{repo = RepoURL, repo_type = faxien,
		   package_type = "app--binary"}.

get_all_package_info([{ErtsVsn, OS, OSURL}|T], Timeout) ->
    AppAndErts = fd_repo_dav_repo_list(OSURL, Timeout),
    lists:foldl(fun("erts.tar.gz", Acc) ->
			ErtsURL = ep_util:join_paths(OSURL, "erts.tar.gz"),
			[#package_info{name = "erts",
				       vsn = ErtsVsn,
				       repo_type = faxien,
				       package_type = erts,
				       path = ErtsURL}|Acc];
		   ("lib", Acc) ->
			LPath = ep_util:join_paths(OSURL, "lib"),
			get_app_and_rel_package_info(ErtsVsn, OS, app,
						     LPath, Timeout) ++ Acc;
		 ("releases", Acc) ->
			RPath = ep_util:join_paths(OSURL, "releases"),
		      get_app_and_rel_package_info(ErtsVsn, OS, release,
						   RPath, Timeout) ++ Acc
	      end, [], AppAndErts) ++
	get_all_package_info(T, Timeout);
get_all_package_info([], _Timeout) ->
    [].

get_app_and_rel_package_info(ErtsVsn, OS, Type, URL, Timeout) ->
    Names = fd_repo_dav_repo_list(URL, Timeout),
    ?INFO("Fetching data for -> ~s  ", [URL]),
    get_app_and_rel_package_info(ErtsVsn, OS, Type, Names, URL, Timeout).

get_app_and_rel_package_info(ErtsVsn, OS, Type, [Name|T], URL, Timeout) ->
    NameURL = ep_util:join_paths(URL, Name),
    Vsns = fd_repo_dav_repo_list(NameURL, Timeout),
    ?INFO(". ", []),
    create_package_info(ErtsVsn, OS, Type, Name, NameURL, Vsns) ++ 
	get_app_and_rel_package_info(ErtsVsn, OS, Type, T, URL, Timeout);
get_app_and_rel_package_info(_ErtsVsn, _OS, _Type, [], _URL, _Timeout) ->
    ?INFO(".~n", []),
    [].
    
create_package_info(ErtsVsn, "Generic", app, Name, NameURL, [Vsn|T]) ->
    PU = package_url(NameURL, Name, Vsn),
    [#package_info{executable_env = "erts-" ++ ErtsVsn, name = Name,
		   vsn = Vsn, package_type = app_binary_generic, path = PU}|
     create_package_info(ErtsVsn, "Generic", app, Name, NameURL, T)];
create_package_info(ErtsVsn, OS, app, Name, NameURL, [Vsn|T]) ->
    PU = package_url(NameURL, Name, Vsn),
    [#package_info{executable_env = "erts-" ++ ErtsVsn, name = Name,
		   vsn = Vsn, package_type = app_binary_specific, path = PU}|
     create_package_info(ErtsVsn, OS, app, Name, NameURL, T)];
create_package_info(ErtsVsn, OS, release, Name, NameURL, [Vsn|T]) ->
    PU = package_url(NameURL, Name, Vsn),
    [#package_info{executable_env = "erts-" ++ ErtsVsn, name = Name,
		   vsn = Vsn, package_type = release, path = PU}|
     create_package_info(ErtsVsn, OS, release, Name, NameURL, T)];
create_package_info(_ErtsVsn, _OS, _Type, _Name, _NameURL, []) ->
    [].
    
package_url(NameURL, Name, Vsn) ->
    ep_util:join_paths(ep_util:join_paths(NameURL, Vsn), Name ++ ".tar.gz").
    

-spec build_os_type_urls(string(), [string()], non_neg_integer()) ->
      [{ErtsVsn::string(), OS::string(), OSURL::string()}].
build_os_type_urls(RepoURL, [ErtsVsn|T], Timeout) ->
    ErtsURL = ep_util:join_paths(RepoURL, ErtsVsn),
    OSTypes = fd_repo_dav_repo_list(ErtsURL, Timeout),
    [{ErtsVsn, "Generic", ep_util:join_paths(ErtsURL, "Generic")}|
     filter_for_arch_specific_os_type_urls(ErtsURL, ErtsVsn, OSTypes)] ++
	build_os_type_urls(RepoURL, T, Timeout);
build_os_type_urls(_RepoURL, [], _Timeout) ->
    [].

filter_for_arch_specific_os_type_urls(ErtsURL, ErtsVsn, OSTypes) ->
    Fun =
	fun("Generic", Acc) ->
		Acc;
	   ("Meta", Acc) ->
		Acc;
	   (OS, Acc) ->
		case os_is_compatible(OS) of
		    true ->
			[{ErtsVsn, OS, ep_util:join_paths(ErtsURL, OS)}|Acc];
		    false ->
			Acc
		end
	end,
    
    lists:foldl(Fun, [], OSTypes).

os_is_compatible(OS) ->
    {CurrentOSMajor, CurrentOSMinor} =
	split_os_major_minor(ewr_util:system_info()),
    {OSMajor, OSMinor} = split_os_major_minor(OS),
    CurrentOSMajor == OSMajor
	andalso 
    list_to_integer(CurrentOSMinor) >= list_to_integer(OSMinor).

split_os_major_minor(OS) ->
    {OSMajor, OSMinor} = shred_to(lists:reverse(OS), $., []),
    {lists:reverse(OSMajor), OSMinor}.
    

shred_to([Char|T], Char, Shredded) ->
    {T, Shredded};
shred_to([H|T], Char, Shredded) ->
    shred_to(T, Char, [H|Shredded]).
    
    

fd_repo_dav_repo_list(URL, Timeout) ->
    try
	{ok, DirContents} = fd_repo_dav:repo_list(URL, Timeout),
        DirContents
    catch
        _C:E ->
            throw(?UEX({repo_connect_failure, {E, URL}},
                       "The repo specified by ~p~ncould not be" ++
                       " communicated with. Check your net connection~n" ++
		       "or try your search without the offending repo.",
                       [URL]))
    end.

%%% Put Functions

publish2("erts", Repos, ErtsVsn, "erts", ErtsVsn,
	 PackageBin, _PackageInfo, Timeout) ->
    Res = return(fd_put:put_erts_package(Repos, ErtsVsn,
					 PackageBin, Timeout)), 
    fd_put:put_erts_checksum_file(Repos, ErtsVsn, PackageBin, Timeout),
    Res;
publish2([$a,$p,$p|_] = Type, Repos, ErtsVsn, AppName, AppVsn,
	 Binary, PackageInfo, Timeout) ->
    % @todo make this transactional - if .app file put fails run a delete.
    [AppTerm] = PackageInfo#package_info.meta,
    AppFileBinary = list_to_binary(lists:flatten(
				     io_lib:fwrite("~p", [AppTerm]))),
    case Type of
	Type when PackageInfo#package_info.os_release == undefined ->
	    fd_put:put_dot_app_file(Repos, ErtsVsn, AppName, AppVsn,
				    AppFileBinary, Timeout), 
	    Res = return(fd_put:put_generic_app_package(Repos, ErtsVsn,
							AppName, AppVsn,
							Binary, Timeout)), 
	    fd_put:put_checksum_file(Repos, ErtsVsn, "lib", AppName,
				     AppVsn, Binary, Timeout),
	    Res;
	Type  -> % Binary application
	    fd_put:put_dot_app_file(Repos, ErtsVsn, AppName, AppVsn,
				    AppFileBinary, Timeout), 
	    Res = return(fd_put:put_binary_app_package(Repos, ErtsVsn,
						       AppName, AppVsn,
						       Binary, Timeout)),
	    fd_put:put_checksum_file(Repos, ErtsVsn, "lib", AppName,
				     AppVsn, Binary, Timeout),
	    Res
    end;
publish2("release", Repos, ErtsVsn, RelName, RelVsn,
	 Binary, PackageInfo, Timeout) ->
    [RelTerm] = PackageInfo#package_info.meta,
    RelFileBinary = list_to_binary(lists:flatten(
				     io_lib:fwrite("~p", [RelTerm]))),
    fd_put:put_dot_rel_file(Repos, ErtsVsn, RelName, RelVsn,
			    RelFileBinary, Timeout),
    Res = return(fd_put:put_release_package(Repos, ErtsVsn,
					    RelName, RelVsn,
					    Binary, Timeout)),
    fd_put:put_checksum_file(Repos, ErtsVsn, "releases",
			     RelName, RelVsn, Binary, Timeout),
    Res.

return({ok, Res}) ->
    Res;
return(Error) ->
    throw(?UEX({publish_failure, Error},
	       "Failed to publish package with error ~p~n",
	       [Error])).
	      
