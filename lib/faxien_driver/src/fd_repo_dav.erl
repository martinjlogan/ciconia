%%%-------------------------------------------------------------------
%%% @copyright Erlware 2007
%%% @author    Martin Logan <martinjlogan@erlware.org>
%%%
%%% @doc This module contains all functions in ewrepo that make requests to a remote repo. *Note* this file should not
%%%      contain any convenience functions or shortcuts.  Those should be placed in higher level modules so that this 
%%%      stays free of any clutter.
%%% @end
%%%
%%% Created :  1 Dec 2007 by Martin Logan <martinjlogan@erlware.org>
%%%-------------------------------------------------------------------
-module(fd_repo_dav).

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
	 	
-export([
   repo_get/2,
   repo_put/3,
   repo_list/2,
   repo_mkcol/2
        ]).
	 	

-include("erlpl.hrl").
-include("eunit.hrl").
	 	

	 	
%%====================================================================
	 	
%% External functions
	 	
%%====================================================================
	 	

	 	
%%-------------------------------------------------------------------
%% @doc
%%  Make the http request to fetch some data.
%%
%% <pre>
%% Example Invocation:
%%  repo_get("http://repo.erlware.org/pub/linux/generic/erts-5.5.5/app--binary/mnesia-3.4.5.epkg", 100000).
%% </pre>
%% @end
%%-------------------------------------------------------------------
-spec repo_get(URL::string(), Timeout::non_neg_integer()) -> {ok, binary()} | {error, term()}.
repo_get([$f,$i,$l,$e,$:,$/,$/|FilePath] = FullURL, Timeout) ->
    ?DEBUG("ewr_repo_dav:repo_get(~p, ~p)~n", [FullURL, Timeout]),
    file:read_file(FilePath);
repo_get([$h,$t,$t,$p,$:,$/,$/|_] = URL, Timeout) ->
    AuthOpts = [],
    repo_get_with_auth(URL, Timeout, AuthOpts);
repo_get([$h,$t,$t,$p,$s,$:,$/,$/|_] = URL, Timeout) ->
    {Domain, _Path} = split_domain_from_path(URL),
    AuthOpts = get_auth_options(Domain),
    repo_get_with_auth(URL, Timeout, AuthOpts).

%%-------------------------------------------------------------------
%% @doc
%%  Put bits onto a filesystem.  This function creates the directory strcuture speficied if it does not exist.
%%
%% <pre>
%% Example Invocation:
%%  repo_put("http://repo.erlware.org/pub/linux/generic/erts-5.5.5/app--binary/mnesia-3.4.5.epkg", <<bin>>, 100000).
%% </pre>
%% @end
%%-------------------------------------------------------------------
-spec repo_put(URL::string(), Payload::binary(), Timeout::non_neg_integer()) -> {ok, binary()} | {error, term()}.
repo_put([$f,$i,$l,$e,$:,$/,$/|FilePath] = FullURL, Payload, _Timeout) ->
    ewl_file:mkdir_p(filename:dirname(FilePath)),
    case file:write_file(FilePath, Payload) of
	ok    -> {ok, FullURL};
	Error -> Error
    end;
repo_put([$h,$t,$t,$p,$:,$/,$/|_] = URL, Payload, Timeout) ->
    AuthOpts = [],
    repo_put_with_auth(URL, Payload, Timeout, AuthOpts);
repo_put([$h,$t,$t,$p,$s,$:,$/,$/|_] = URL, Payload, Timeout) ->
    {Domain, _Path} = split_domain_from_path(URL),
    AuthOpts = get_auth_options(Domain),
    repo_put_with_auth(URL, Payload, Timeout, AuthOpts).

%%-------------------------------------------------------------------
%% @doc
%% Individually creates each collection required by the Path. Has the syntax of mkdir -p but over webdav.
%% @end
%%-------------------------------------------------------------------
-spec repo_mkcol(URL::string(), Timeout::non_neg_integer()) -> ok | {error, term()}.
repo_mkcol([$f,$i,$l,$e,$:,$/,$/|FilePath], _Timeout) ->
    try ewl_file:mkdir_p(FilePath) 
    catch
  _C:E ->
      {error, E}
    end;
repo_mkcol([$h,$t,$t,$p,$:,$/,$/|_] = URL, Timeout) ->
    AuthOpts = [],
    repo_mkcol_with_auth(URL, Timeout, AuthOpts);
repo_mkcol([$h,$t,$t,$p,$s,$:,$/,$/|_] = URL, Timeout) ->
    {Domain, _Path} = split_domain_from_path(URL),
    AuthOpts = get_auth_options(Domain),
    repo_mkcol_with_auth(URL, Timeout, AuthOpts).

%%-------------------------------------------------------------------
%% @doc
%% Return a the contents of a directory.
%% @end
%%-------------------------------------------------------------------
-spec repo_list(URL::string(), Timeout::non_neg_integer()) ->
    {ok, DirContents::list()} | {error, Reason::term()}.
repo_list([$f,$i,$l,$e,$:,$/,$/|FilePath] = FP, _Timeout) ->
    FullPath = filename:join([FilePath, "*"]), 
    try
	{ok, [filename:basename(E) || E <- filelib:wildcard(FullPath)]}
    catch
	_C:_E ->
	    {error,{repo_list, FP}}
    end;
repo_list([$h,$t,$t,$p,$:,$/,$/|_] = URL, Timeout) ->
    repo_list_with_auth(URL, Timeout, []);
repo_list([$h,$t,$t,$p,$s,$:,$/,$/|_] = URL, Timeout) ->
    {Domain, _Path} = split_domain_from_path(URL),
    AuthOpts = get_auth_options(Domain),
    repo_list_with_auth(URL, Timeout, AuthOpts).


%%%===================================================================
%%% Internal functions
%%%===================================================================

repo_list_with_auth(URL, Timeout, AuthOpts) ->
    Opts = [{"Connection", "TE"},
	    {"TE", "trailers"},
	    {"Depth", "1"},
	    {"Content-Type", "application/xml"}],
    case catch ibrowse:send_req(URL, Opts, propfind, "", AuthOpts, Timeout) of
        {ok, "207", _, Body} -> 
	    {ok, parse_out_package_versions(Body)};
	{ok, Code, _, _} -> 
	    {error, {"No list found. http code: ", Code}};
        {error, _Reason} = Res -> 
	    Res;
        {'EXIT', Reason} -> 
            {error, Reason}
    end.
    
parse_out_package_versions(Body) ->
    {Elem, _} = xmerl_scan:string(Body),
    [filename:basename(E) || E <- tl(lists:sort(xmerl_xs:value_of(xmerl_xs:select("//D:href", Elem))))].

handle_ibrowse_return(Result, AcceptableCodes) ->
    case Result of
	{ok, Code, _, Payload} -> 
	    case lists:member(Code, AcceptableCodes) of
		true -> 
		    {ok, Payload};
		false ->
		    ?DEBUG("ewr_repo_dav:handle_ibrowse_return/2 -> ~p~n", [Result]),
		    {error, {http_return_code, Code}}
	    end;
        Error = {error, _} -> 
	    ?DEBUG("ewr_repo_dav:handle_ibrowse_return/2 -> ~p~n", [Result]),
	    Error
    end.

repo_get_with_auth(URL, Timeout, AuthOpts) ->
    ?DEBUG("ewr_repo_dav:repo_get(~p, ~p)~n", [URL, Timeout]),
    Res = ibrowse:send_req(URL, [], get, [], AuthOpts, Timeout),
    handle_ibrowse_return(Res, ["200"]).

repo_put_with_auth(URL, Payload, Timeout, AuthOpts) ->
    % Creates the directory structure within the repo.
    repo_mkcol(filename:dirname(URL), Timeout),
    ?DEBUG("ewr_repo_dav:repo_put putting to ~p~n", [URL]),
    Res = (catch ibrowse:send_req(URL, [], put, Payload, AuthOpts, Timeout)),
    case handle_ibrowse_return(Res, ["200", "201"]) of
        {ok, _} -> {ok, URL};
        Error   -> Error
    end.

repo_mkcol_with_auth(FullURL, Timeout, AuthOpts) ->
    {Domain, Path} = split_domain_from_path(FullURL),
    (catch lists:foldl(fun(PathElement, Acc) -> 
			       NewAcc = Acc ++ PathElement ++ "/",
			       URL    = ewl_file:join_paths(Domain, NewAcc),
			       ?DEBUG("mkcol on ~p~n", [URL]),
			       % In place for the build in logging
			       handle_ibrowse_return(
				 ibrowse:send_req(URL, [], mkcol, [], AuthOpts, Timeout),
				 ["200", "201"]),
			       NewAcc
		       end, [], string:tokens(Path, "/"))),
    ok.

split_domain_from_path(URL) ->
    split_domain_from_path(URL, []).
    
split_domain_from_path([$.|T], DAcc) ->
    pull_to_next_slash(T, [$.|DAcc]);
split_domain_from_path([H|T], DAcc) ->
    split_domain_from_path(T, [H|DAcc]).

pull_to_next_slash([$/|T], DAcc) ->
    {lists:reverse(DAcc), T}; 
pull_to_next_slash([H|T], DAcc) ->
    pull_to_next_slash(T, [H|DAcc]).

get_auth_options(Repo) ->
    AuthFile = filename:join(home_dir(), ".faxien.auth"),
    case file:consult(AuthFile) of
        {ok, Terms} ->
            lookup_url(Repo, Terms);
        {error, Error} ->
            ?DEBUG("Could not find auth options for repo~p (reason: ~p)~n", [Repo, Error]),
            []
    end.

lookup_url(URL, TermList) ->
    case proplists:get_value(auth, TermList) of
        PropList when is_list(PropList) ->
            get_auth_for_url(URL, PropList);
        undefined ->
            ?DEBUG("Missing faxien_secrets in secrets file for repo ~p~n", [URL]),
            [];
        _Other ->
            ?DEBUG("Wrong format for faxien_secrets in secrets file for repo ~p: ~p~n", [URL, _Other]),
            []
    end.

get_auth_for_url(URL, PropList) ->
    case [Tuple || {K, _} = Tuple <- PropList, lists:prefix(K, URL)] of
        [] = L ->
            ?WARN("No auth options for repo ~p~n", [URL]),
            L;
        [{_K, AuthOpts}] ->
            check_ssl(AuthOpts),
            AuthOpts;
        [{_K, AuthOpts}|_] ->
            ?WARN("More than one matching auth option for repo ~p, first one used~n", [URL]),
            check_ssl(AuthOpts),
            AuthOpts
    end.

home_dir() ->
    case os:getenv("HOME") of
        undefined ->
            ?DEBUG("The HOME environment variable is not set~n", []),
            "."; % Default to current dir
        Home ->
            Home
    end.

check_ssl(AuthOpts) when is_list(AuthOpts) ->
    case proplists:get_value(is_ssl, AuthOpts) of
        true ->
            start_ssl();
        _ ->
            ok
    end.

start_ssl() ->
    case application:start(ssl) of
        {error,{already_started,_}} ->
            ok;
        {error, Reason} = Error ->
            ?DEBUG("Failed to start ssl, error:~n~p~n", [Reason]),
            Error;
        ok ->
            ssl:seed(term_to_binary(make_ref())),
            ok
    end.

