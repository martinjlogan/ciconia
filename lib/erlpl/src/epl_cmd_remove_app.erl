%%%-------------------------------------------------------------------
%%% @author Martin Logan <martinjlogan@Macintosh.local>
%%% @copyright (C) 2010, Martin Logan
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
-module(epl_cmd_remove_app).

%% API
-export([run/2, spec/0, description/0]).

-include("erlpl.hrl").

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Remove application(s)
%% @spec (AppName, Options) -> ok
%% where
%%  Options = [Option]
%%   Option = {force, bool()} 
%% @end
%%--------------------------------------------------------------------
run(AppName, Options) ->
    ?DEBUG("AppName ~p Options ~p", [AppName, Options]),
    RootDir = epl_util:get_option(root_dir, Options, spec(), required),
    AppVsns = get_app_vsns(AppName, RootDir, Options),
    handle_remove_apps(RootDir, AppName, AppVsns, Options).

description() ->
    "remove an application package".

-spec spec() -> get_opts_spec().
spec() ->
    CmdLnTail = "<pkg_dir>",
    OptionsTail =
	[{"pkg_dir", "path to the application package to be removed"}],
    OptionSpecs =
	[
      %% {Name,   ShortOpt, LongOpt,        ArgSpec,               HelpMsg}
	 {verbose, $v,"verbose", undefined, "Verbose output"},
	 {root_dir, $d,  "root_dir", string,
	  "The root dir where Erlang is installed"},
	 {version, $n,  "version",      string, "App version number"},
	 {force, $f,  "force", {atom, false},
	  "Forces the command to run and eliminates all prompts"}
	],
    {OptionSpecs, CmdLnTail, OptionsTail}.


%%%---------------------------------------------------------
%%% Internal Functions
%%%---------------------------------------------------------

handle_remove_apps(RootDir, AppName, AppVsns, Options) ->
    Prompt = io_lib:fwrite("Do you want to remove app(s)~n~p~n(y/n)?", [[{AppName, AppVsn} || AppVsn <- AppVsns]]), 
    RespSet = ["y", "n"],
    SuccessSet = ["y"],
    Fun = fun() -> remove_apps(RootDir, AppName, AppVsns) end,
    epl_util:force_or_prompt_for_an_action(Prompt, RespSet, SuccessSet, Fun, Options).

remove_apps(RootDir, AppName, AppVsns) ->
    try
	[ewl_file:delete_dir(epl_installed_paths:app_dir(RootDir, AppName, AppVsn)) || AppVsn <- AppVsns]
    catch
	_C:E ->
	    Msg = "Failed to delete one of the applications.~n~p~nCheck to see that you have proper permissions~n",
	    Vals = [[{AppName, AppVsn} || AppVsn <- AppVsns]],
	    throw(?UEX({failed_to_remove_app, E, AppName}, Msg, Vals))
    end.
    
get_app_vsns(AppName, RootDir, Options) ->
    case epl_util:get_val(version, Options) of
	undefined ->
	    epl_root_dir_util:list_app_vsns(RootDir, AppName);
	Vsn ->
	    [Vsn]
    end.
		
