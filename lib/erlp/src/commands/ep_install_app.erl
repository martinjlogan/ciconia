%%%-------------------------------------------------------------------
%%% @author Martin Logan
%%% @copyright (C) 2010, Erlware
%%% @doc
%%%  All functions in command modules run in two modes. One with
%%%  prompts for a user and the other forced with no prompts for use
%%%  programatically.
%%%
%%%  It is within this module that all package validation takes place.
%%%  Business logic/policy are to be implemented here and will leverage
%%%  policy neutral driver functions to do the actual work.
%%% @end
%%% Created : 16 Jun 2010 by Martin Logan <martinjlogan@Macintosh.local>
%%%-------------------------------------------------------------------
-module(ep_install_app).

%% API
-export([run/2, spec/0, description/0]).

-include("erlpl.hrl").
-include("eunit.hrl").

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc List installed packages.
%% @spec (Options) -> ok
%% where
%%  Options = [Option]
%%   Option = {force, bool()}
%% @end
%%--------------------------------------------------------------------
run(AppName, Options) ->
    install_app(AppName, Options).

description() ->
    "Install an Erlang application package".

-spec spec() -> get_opts_spec().
spec() ->
    CmdLnTail = "",
    OptionsTail = "",
    OptionSpecs =
	[
      %% {Name,   ShortOpt, LongOpt,        ArgSpec,           HelpMsg}
	 {verbose, $v, "verbose", undefined, "Option for verbose output"},
	 {root_dir,$d, "root_dir", string,"The root dir for the installation"},
	 {version, $n, "version", string, "The application version number"},
	 {force,   $f, "force", undefined,
	  "Forces the command to run and eliminates all prompts"},
	 {timeout, $t, "timeout", {integer, 60000},
	  "The timeout value for the operation"}
	],
    {OptionSpecs, CmdLnTail, OptionsTail}.

%%%===================================================================
%%% Internal Functions
%%%===================================================================
install_app(AppName, Options) ->
    epl_util:assert_option(root_dir, Options),
    epl_util:assert_option(timeout, Options),
    AppList = ep_cache:fetch(AppName, application, Options),
    ep_install_util:ep_cache_fetch_ensure(AppName, AppList),
    AppData = select_app_to_install(AppList, Options),
    AppBinary = fetch_app_binary(AppData, Options),
    AppPackagePath = ep_install_util:write_out_package(AppData, AppBinary),
    epl_install_app:run(AppPackagePath, Options),
    epl_file:remove(AppPackagePath, [recursive]). 
    

fetch_app_binary([], _Options) ->
    throw(?UEX(app_not_found,
	       "The application you were looking for could not be found~n" ++
	       "Try updating your package list with erlp update-cache",
	       []));
fetch_app_binary(AppData, Options) ->
    ep_install_util:fetch_binary(AppData, Options).

select_app_to_install([], _Options) ->
    throw(?UEX(not_found,
	       "The application you are trying to install could not" ++
	       " be found in the repos~n",
	       []));
select_app_to_install(AppList, Options) ->
    AppVsn = epl_util:get_option(version, Options, spec(), optional),
    select_app_to_install2(AppVsn, AppList).

select_app_to_install2(undefined, AppList) ->
    Hd = hd(AppList),
    AppVsn = Hd#package_info.vsn,
    app_search_strategy(
      select_app_to_install_by_vsn(AppVsn, AppList));
select_app_to_install2(AppVsn, AppList) ->
    app_search_strategy(
      select_app_to_install_by_vsn(AppVsn, AppList)).

select_app_to_install_by_vsn(AppVsn, [AppData|T]) ->
    case AppData#package_info.vsn of
	AppVsn ->
	    [AppData|select_app_to_install_by_vsn(AppVsn, T)];
	_ ->
	    select_app_to_install_by_vsn(AppVsn, T)
    end;
select_app_to_install_by_vsn(_AppVsn, []) ->
    [].

%% TODO add in a real strategy here
app_search_strategy([]) ->
    throw(?UEX(app_not_found,
	       "The application you are trying to install can't be found~n" ++
	       "Please check your net connection. If that is working try~n" ++
	       "running erlp update-cache -r <repo-url>~n",
	       []));
app_search_strategy(AppList) ->
    hd(AppList).

%%%===================================================================
%%% Test Functions
%%%===================================================================
