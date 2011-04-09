%%-------------------------------------------------------------------
%%% @author Martin Logan <martinjlogan@Macintosh.local>
%%% @copyright (C) 2010, Martin Logan
%%% @doc
%%%  This is a test command. It is a dummy example.
%%% @end
%%% Created : 16 Jun 2010 by Martin Logan <martinjlogan@Macintosh.local>
%%%-------------------------------------------------------------------
-module(epl_cmd_test).

%% API
-export([run/1, error/1, spec/0, description/0]).

-include("erlpl.hrl").

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc dummy command
%% @end
%%--------------------------------------------------------------------
run(_Arg) ->
    ok.

error(_Error) ->
    "who knows what happened?~n".

description() ->
    "a dummy test command".

-spec spec() -> get_opts_spec().
spec() ->
    Repo = "http://repo.erlware.org/pub",
    OptionSpecs =
	[
      %% {Name,     ShortOpt,  LongOpt,        ArgSpec,               HelpMsg}
	 {help,      $?,        "help",         undefined,             "Show erlp help"},
	 {package,   $p,        "package",      string,                "Package name"},
	 {version,   $v,        "version",      string,                "Package version"},
	 {repo,      $r,        "repo",         {string, Repo},        "Repo URL"},
	 {force,     $f,        "force",        {atom, false},         "Forces the command to run and eliminates all prompts"}
	],
    {OptionSpecs, undef, undef}.

