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
write(PackageInfo, Options) when is_tuple(PackageInfo) ->
    write([PackageInfo], Options);
write(PackageList, Options) ->
    CacheFilePath = cache_file_path(Options),
    ewl_file:mkdir_p(filename:dirname(CacheFilePath)),
    Dict = fetch(Options),
    #package_info{repo = Repo} = hd(PackageList),
    NewDict = clean_dict(Repo, Dict),
    epl_file:write_term(CacheFilePath, 
			epl_util:dictafy(PackageList, NewDict)).
%% @doc fetch a list of packages from the cache based on the name, vsn 
%% and type of the package.
-spec fetch(Name::string(), Vsn::string(),
		   release | application | erts, [tuple()]) -> list().
fetch(Name, Vsn, Type, Options) ->
    lists:filter(
      fun(PackageInfo) ->
	      case Type of
		  release ->
		      lists:member(PackageInfo#package_info.package_type,
				   ?RELEASE_PACKAGE_TYPE_IDS);
		  application ->
		      lists:member(PackageInfo#package_info.package_type,
				   ?APP_PACKAGE_TYPE_IDS);
		  erts ->
		      lists:member(PackageInfo#package_info.package_type,
				   ?ERTS_PACKAGE_TYPE_IDS)
	      end
		  andalso
		  PackageInfo#package_info.vsn == Vsn
      end,
      fetch(Name, Options)).

%% @doc fetch a list of packages from the cache based on the name
%% and type of the package.
-spec fetch(Name::string(), release | application | erts, [tuple()]) -> list().
fetch(Name, Type, Options) ->
    lists:filter(
      fun(PackageInfo) ->
	      case Type of
		  release ->
		      lists:member(PackageInfo#package_info.package_type,
				   ?RELEASE_PACKAGE_TYPE_IDS);
		  application ->
		      lists:member(PackageInfo#package_info.package_type,
				   ?APP_PACKAGE_TYPE_IDS);
		  erts ->
		      lists:member(PackageInfo#package_info.package_type,
				   ?ERTS_PACKAGE_TYPE_IDS)
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

clean_dict(Repo, Dict) ->
    DictList = dict:to_list(Dict),
    dict:from_list(clean_key_val(Repo, DictList)).

clean_key_val(Repo, [{Key, Row}|T]) ->
    [{Key, clean_row(Repo, Row)}|clean_key_val(Repo, T)];
clean_key_val(_Repo, []) ->
    [].

clean_row(Repo, [#package_info{repo = Repo}|T]) ->
    clean_row(Repo, T);
clean_row(Repo, [PackageInfo|T]) ->
    [PackageInfo|clean_row(Repo, T)];
clean_row(_Repo, []) ->
    [].
    
%%%===================================================================
%%% Testing Functions
%%%===================================================================

-ifndef(NOTEST).
-include_lib("eunit/include/eunit.hrl").

clean_dict_test() ->
    Given = [
		#package_info{name = faxien, vsn = "2.2.3-rc2"},
		#package_info{name = faxien, vsn = "2.2.3", repo = "a"},
		#package_info{name = faxien, vsn = "2.2.3-rc1"},
		#package_info{name = faxien, vsn = "1.2.3", repo = "a"},
		#package_info{name = faxien, vsn = "1.2.3-rc1"}
	       ],

    Expected = [
		#package_info{name = faxien, vsn = "2.2.3-rc2"},
		#package_info{name = faxien, vsn = "2.2.3-rc1"},
		#package_info{name = faxien, vsn = "1.2.3-rc1"}
	       ],

    ?assertMatch(Expected,
		 dict:fetch(faxien, clean_dict("a", epl_util:dictafy(Given)))).

-endif.
