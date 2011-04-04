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
-module(epl_managed).

%% API
-export([run/1, spec/0, description/0]).

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
run(Options) ->
    epl_util:assert_option(meta_dir, Options),
    App     = epl_util:get_val(app, Options),
    Release = epl_util:get_val(release, Options),
    pretty_print_app(App, Release, Options),
    pretty_print_release(App, Release, Options).


description() ->
    "list packages under management".

-spec spec() -> get_opts_spec().
spec() ->
    CmdLnTail = "",
    OptionsTail = "",
    OptionSpecs =
	[
      %% {Name,   ShortOpt, LongOpt,        ArgSpec,           HelpMsg}
	 {verbose, $v,  "verbose", undefined, "Verbose output"},
	 {app, $a,  "app",undefined, "List only applications"},
	 {release, $r,  "release", undefined, "List only releases"},
	 {everything, $e, "everything", undefined, "List everything in the db"}
	],
    {OptionSpecs, CmdLnTail, OptionsTail}.


%%%---------------------------------------------------------
%%% Internal Functions
%%%---------------------------------------------------------
pretty_print_app(App, Rel, Options)
  when App =:= true; App =:= undefined andalso Rel =:= undefined ->
    ?INFO("~nApplications Under Management:~n", []),
    ?INFO("------------------------------~n", []),
    Records = epl_installed_info:all_apps(Options),
    epl_list:print_installed([{A, B} || #package_info{name = A, vsn = B} <-
					    Records]);
pretty_print_app(_App, _Rel, _Options) ->
    ok.
    
pretty_print_release(App, Rel, Options)
  when Rel =:= true; App =:= undefined andalso Rel =:= undefined ->
    ?INFO("~nReleases Under Management:~n", []),
    ?INFO("--------------------------~n", []),
    Records = epl_installed_info:all_releases(Options),
    epl_list:print_installed([{A, B} || #package_info{name = A, vsn = B} <-
					    Records]);
pretty_print_release(_App, _Rel, _Options) ->
    ok.

