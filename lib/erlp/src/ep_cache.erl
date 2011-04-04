%%%-------------------------------------------------------------------
%%% @author Martin Logan <martinjlogan@Macintosh.local>
%%% @copyright (C) 2011, Martin Logan
%%% @doc
%%%  A wrapper for the epl_cache for remote caching.
%%% @end
%%% Created : 12 Mar 2011 by Martin Logan <martinjlogan@Macintosh.local>
%%%-------------------------------------------------------------------
-module(ep_cache).

%% API
-export([
	 write/2,
	 fetch/1,
	 fetch/2,
	 fetch/3,
	 fetch/4
	]).

-include("erlpl.hrl").

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Write cache data out to the cache.
-spec write(list() | tuple(), option_list()) -> ok.
write(PackageList, Options) ->
    CacheFilePath = cache_file_path(Options),
    ewl_file:mkdir_p(filename:dirname(CacheFilePath)),
    epl_file:write_term(CacheFilePath, 
			epl_util:dictafy(PackageList)).

%% @doc fetch a list of packages from the cache based on the name, vsn 
%% and type of the package.
-spec fetch(Name::string(), Vsn::string(),
		   release | application | erts, [tuple()]) -> list().
fetch(Name, Vsn, Type, Options) ->
    lists:filter(fun(PackageInfo) ->
			 case Type of
			     release ->
				 lists:member(PackageInfo#package_info.package_type, ?RELEASE_PACKAGE_TYPE_IDS);
			     application ->
				 lists:member(PackageInfo#package_info.package_type, ?APP_PACKAGE_TYPE_IDS);
			     erts ->
				 lists:member(PackageInfo#package_info.package_type, ?ERTS_PACKAGE_TYPE_IDS)
			 end
			     andalso
			 PackageInfo#package_info.vsn == Vsn
		 end,
		 fetch(Name, Options)).

%% @doc fetch a list of packages from the cache based on the name
%% and type of the package.
-spec fetch(Name::string(), release | application | erts, [tuple()]) -> list().
fetch(Name, Type, Options) ->
    lists:filter(fun(PackageInfo) ->
			 case Type of
			     release ->
				 lists:member(PackageInfo#package_info.package_type, ?RELEASE_PACKAGE_TYPE_IDS);
			     application ->
				 lists:member(PackageInfo#package_info.package_type, ?APP_PACKAGE_TYPE_IDS);
			     erts ->
				 lists:member(PackageInfo#package_info.package_type, ?ERTS_PACKAGE_TYPE_IDS)
			 end
		 end,
		 fetch(Name, Options)).

%% @doc fetch all the package specs for a given package name.
-spec fetch(string(), option_list()) -> list().
fetch(Name, Options) ->
    case dict:find(Name, fetch(Options)) of
	{ok, Value} ->
	    Value;
	error ->
	    []
    end.

%% @doc fetch the raw cache term.
-spec fetch(option_list()) -> term().
fetch(Options) ->
    ErlplMetaDir = epl_util:get_option(meta_dir, Options),
    CacheFile = cache_file_path(Options),
    case filelib:is_file(CacheFile) of
	true ->
	    epl_file:consult(CacheFile);
	false ->
	    ewl_file:mkdir_p(ErlplMetaDir),
	    dict:new()
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================
cache_file_path(Options) ->
    MetaDir = epl_util:get_option(meta_dir, Options),
    filename:join(MetaDir, "repo_cache.erlp").

