%%%-------------------------------------------------------------------
%%% @author Martin Logan <martinjlogan@Macintosh.local>
%%% @copyright (C) 2011, Martin Logan
%%% @doc
%%%
%%% @end
%%% Created : 21 Mar 2011 by Martin Logan <martinjlogan@Macintosh.local>
%%%-------------------------------------------------------------------
-module(agner_driver).

%% API
-export([
	 %put/4,
	 %get/2,
	 list/2
	]).

-include("erlpl.hrl").

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
list(_Repos, Timeout) ->
    AgnerRepos = agner_repo_list(Timeout),
    ?DEBUG("agner repos are ~p~n", [AgnerRepos]),
    fetch_package_info_for_all_repos(AgnerRepos, Timeout).


%%%===================================================================
%%% Internal functions
%%%===================================================================
%% Fetch a list of urls for all agner repos in the github based repo
agner_repo_list(Timeout) ->
    URL = "http://github.com/api/v2/json/organizations" ++
	"/agner/public_repositories",
    ReposJson = get(URL, Timeout),
    {Obj, _, _} = ktj_parse:parse(ReposJson),
    {obj,[{<<"repositories">>, ObjList}]} = Obj,
    lists:map(fun({obj, KeyVals}) ->
		      {value, {<<"url">>, RepoUrl}} =
			  lists:keysearch(<<"url">>, 1, KeyVals),
		      binary_to_list(RepoUrl)
	      end,
	      ObjList).

fetch_package_info_for_all_repos([AgnerRepo|T], Timeout) ->
    AgnerRepoBlobURL = github_all_blob_url(AgnerRepo),
    ConfigHash = fetch_github_blob_value(<<"agner.config">>, AgnerRepoBlobURL, Timeout),
    timer:sleep(1500),
    case ConfigHash of
	undefined -> 
	    ?INFO("skipping ~s~n", [AgnerRepo]);
	ConfigHash ->
	    AgnerConfigURL = github_hash_based_url(AgnerRepo, ConfigHash),
	    timer:sleep(1500),
	    ProjectURLStruct = project_url(AgnerConfigURL, Timeout),
	    ?DEBUG("project_url ~p~n", [ProjectURLStruct]),
	    timer:sleep(1500),
	    %% XXX now need to fetch all versions available and parse the url struct in a more robust way
	    %% after that I need to store hash urls for the blob set at that version. Then write
	    %% an http or git based way to pull the full project down on an install. 
	    AppVersions =
		fetch_app_file_versions(ProjectURLStruct, Timeout),
	    ?DEBUG("app versions found ~p~n", [AppVersions])
    end,
    fetch_package_info_for_all_repos(T, Timeout);
fetch_package_info_for_all_repos([], _Timeout) ->
    [].

github_all_blob_url(Repo) ->
    UserAndRepo = github_user_and_repo(Repo),
    "http://github.com/api/v2/json/blob/all/" ++
	UserAndRepo ++ "/master".

github_hash_based_url(Repo, ConfigHash) when is_binary(ConfigHash) ->
    github_hash_based_url(Repo, binary_to_list(ConfigHash));
github_hash_based_url(Repo, ConfigHash) ->
    UserAndRepo = github_user_and_repo(Repo),
    "http://github.com/api/v2/json/blob/show/" ++ UserAndRepo ++ "/" ++ ConfigHash.

%% XXX cleanup the algorythem that works here. Make it smoother.
%% add in support for branches and other repos. Add in more slowdown.
fetch_app_file_versions({url, {git, Repo, _}}, Timeout) ->
    AppHash = fetch_app_file_hash(Repo, Timeout),
    AppURL = github_hash_based_url(Repo, AppHash),
    AppFileStr = get(AppURL, Timeout),
    {ok, Toks, _} = erl_scan:string(AppFileStr),
    {ok, {application, _Name, Tuples}} = erl_parse:parse_term(Toks),
    {value, {vsn, Vsn}} = lists:keysearch(vsn, 1, Tuples),
    Vsn;
fetch_app_file_versions(_, _Timeout) ->
    unsupported_repo_type.
    
fetch_app_file_hash(Repo, Timeout) ->
    HashURL = github_all_blob_url(Repo),
    {Obj, _, _} = ktj_parse:parse(get(HashURL, Timeout)),
    search_blobs(".*\\.app", Obj).

project_url(ConfigURL, Timeout) ->
    ConfigStr = get(ConfigURL, Timeout),
    try 
	parse_out_project_repo_url(ConfigStr)
    catch
	_C:_E ->
	    undefined
    end.
    
-spec parse_out_project_repo_url(string()) -> term().
parse_out_project_repo_url(ConfigString) ->
    ConfigStringMod =
	"[" ++
	re:replace(ConfigString, "}\\.", "},", [{return, list}, global]) ++
	"a].",
    {ok, Toks, _} = erl_scan:string(ConfigStringMod),
    {ok, ConfigTerm} = erl_parse:parse_term(Toks),
    case lists:keysearch(url, 1, ConfigTerm) of
	{value, Tuple} ->
	    Tuple;
	undefined ->
	    undefined
    end.
	    
fetch_github_blob_value(BlobKeyOrPattern, HashURL, Timeout) ->
    {Obj, _, _} = ktj_parse:parse(get(HashURL, Timeout)),
    search_blobs(BlobKeyOrPattern, Obj).

get(URL, Timeout) ->
    try
	{ok, "200", _, Payload} = ibrowse:send_req(URL, [], get, [], [], Timeout),
	Payload
    catch
	_C:E ->
	    throw(?UEX({http_request_failure, {URL, E}},
		       "Failed on a GET to ~s~nCheck your connection to the network~n",
		       [URL]))
    end.

search_blobs(Key, {obj, [{<<"blobs">>, {obj, BlobList}}]})
  when is_binary(Key) ->
    case lists:keysearch(Key, 1, BlobList) of
	{value, {_, Blob}} -> Blob;
	_ -> undefined
    end;
search_blobs(Regexp, {obj, [{<<"blobs">>, {obj, BlobList}}]}) ->
    search_blobs_by_regexp(Regexp, BlobList);
search_blobs(_Key, Resp) ->
    throw(?UEX(bad_response_from_agner_repo,
	       "bad response from the agner repo~n" ++
	       "please check that the repo you used is valid~n",
	       [Resp])).

search_blobs_by_regexp(Regexp, [{Key, Value}|T]) ->
    case re:run(binary_to_list(Key), Regexp) of
	{match, _} ->
	    Value;
	_ ->
	    search_blobs_by_regexp(Regexp, T)
    end;
search_blobs_by_regexp(_Regexp, []) ->
    undefined.

github_user_and_repo([_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_|T]) ->
    lists:reverse(strip_dot_git(lists:reverse(T))).

strip_dot_git([$t,$i,$g,$.|T]) ->
    T;
strip_dot_git(Str) ->
    Str.

