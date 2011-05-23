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
list(Repos, Timeout) ->
    AgnerRepos = agner_repo_list(Timeout),
    ?DEBUG("agner repos are ~p~n", [AgnerRepos]),
    fetch_package_info(AgnerRepos, Timeout).


%%%===================================================================
%%% Internal functions
%%%===================================================================

fetch_package_info(AgnerRepos, Timeout) ->
    lists:foreach(fun(Repo) ->
			  file:set_cwd("/tmp"),
			  % TODO make sure git is present
			  Hash = repo_hash(Repo, Timeout),
			  timer:sleep(1500),
			  case Hash of
			      undefined -> 
				  io:format("skipping ~s~n", [Repo]);
			      Hash ->
				  ConfigURL = project_url(Repo, Hash, Timeout),
				  AppVersions =
				      fetch_app_file_versions(ConfigURL,
							      Timeout),
				  io:format("~p~n", [AppVersions])
			  end
		  end,
		  AgnerRepos).

%% XXX cleanup the algorythem that works here. Make it smoother.
%% add in support for branches and other repos. Add in more slowdown.
fetch_app_file_versions({url, {git, Repo, _}}, Timeout) ->
    AppHash = fetch_app_file_hash(Repo, Timeout),
    AppURL = build_hash_based_url(Repo, AppHash),
    AppFileStr = get(AppURL, Timeout),
    {ok, Toks, _} = erl_scan:string(AppFileStr),
    {ok, {application, _Name, Tuples}} = erl_parse:parse_term(Toks),
    {value, {vsn, Vsn}} = lists:keysearch(vsn, 1, Tuples),
    Vsn;
fetch_app_file_versions(_, _Timeout) ->
    unsupported_repo_type.
    
fetch_app_file_hash(Repo, Timeout) ->
    HashURL = build_hash_url(Repo),
    {Obj, _, _} = ktj_parse:parse(get(HashURL, Timeout)),
    search_blobs(".*\\.app", Obj).

project_url(Repo, Hash, Timeout) ->
    HashStr = binary_to_list(Hash),
    ConfigURL =
	build_hash_based_url(Repo, HashStr),
    ConfigStr = get(ConfigURL, Timeout),
    Config =
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

	    
			      

repo_hash(Repo, Timeout) ->
    HashURL = build_hash_url(Repo),
    {Obj, _, _} = ktj_parse:parse(get(HashURL, Timeout)),
    agner_config_hash(Obj).
    
build_hash_based_url(Repo, Hash) when is_binary(Hash) ->
    build_hash_based_url(Repo, binary_to_list(Hash));
build_hash_based_url(Repo, Hash) ->
    UserAndRepo = github_user_and_repo(Repo),
    "http://github.com/api/v2/json/blob/show/" ++ UserAndRepo ++ "/" ++ Hash.

build_hash_url(Repo) ->
    UserAndRepo = github_user_and_repo(Repo),
    "http://github.com/api/v2/json/blob/all/" ++
	UserAndRepo ++ "/master".

agner_config_hash(Json) ->
    search_blobs(<<"agner.config">>, Json).

search_blobs(Key, {obj, [{<<"blobs">>, {obj, BlobList}}]})
  when is_binary(Key) ->
    case lists:keysearch(Key, 1, BlobList) of
	{value, {_, Blob}} -> Blob;
	_ -> undefined
    end;
search_blobs(Regexp, {obj, [{<<"blobs">>, {obj, BlobList}}]}) ->
    search_blogs_by_regexp(Regexp, BlobList);
search_blobs(_Key, Resp) ->
    throw(?UEX(bad_response_from_agner_repo,
	       "bad response from the agner repo~n" ++
	       "please check that the repo you used is valid~n",
	       [Resp])).

search_blogs_by_regexp(Regexp, [{Key, Value}|T]) ->
    case re:run(binary_to_list(Key), Regexp) of
	{match, _} ->
	    Value;
	_ ->
	    search_blogs_by_regexp(Regexp, T)
    end;
search_blogs_by_regexp(_Regexp, []) ->
    undefined.
	    
    
	
    

github_user_and_repo([_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_|T]) ->
    lists:reverse(strip_dot_git(lists:reverse(T))).

strip_dot_git([$t,$i,$g,$.|T]) ->
    T;
strip_dot_git(Str) ->
    Str.

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
