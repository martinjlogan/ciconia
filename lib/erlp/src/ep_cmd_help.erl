%%%-------------------------------------------------------------------
%%% @author Martin Logan <martinjlogan@Macintosh.local>
%%% @copyright (C) 2011, Martin Logan
%%% @doc
%%%   This module prints help information for erlp.
%%% @end
%%% Created : 20 Mar 2011 by Martin Logan <martinjlogan@Macintosh.local>
%%%-------------------------------------------------------------------
-module(ep_cmd_help).

%% API
-export([run/1, error/1, spec/0, description/0]).

-include("erlpl.hrl").
-include("eunit.hrl").

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Print help information
%% @spec (Options) -> ok
%% where
%%  Options = [Option]
%%   Option = {force, bool()} 
%% @end
%%--------------------------------------------------------------------
run(Options) ->
    help_commands(epl_util:get_val(commands, Options), Options).

help_commands(true, _Options) ->
    ?INFO("This command is not yet implemented. Here is a basic~n" ++
	  "list. Run erlp help --command <command> on each for~n" ++
	  "more info on each command.~n~n" ++
	  "  install-release~n" ++
	  "  install-app~n" ++
	  "  publish~n" ++
	  "  update-cache~n" ++
	  "  search~n" ++
	  "  help~n~n", []);
help_commands(_False, Options) ->
    help_command(epl_util:get_val(command, Options), Options).

help_command(undefined, _Options) ->
    help();
help_command(Command, Options) ->
    epl_cmdln_lib:print_usage_for_command(Command, Options).

help() ->
    Msg =
	"erlp is serious package management for Erlang/OTP This is a~n" ++
	"basic help message containing pointers to more information.~n" ++
	"~nUsage:~n"
	"  erlp help --commands~n" ++
	"  erlp version~n" ++
	"  erlp <command> [options] [arguments]~n" ++
	"~nExamples:~n" ++
	"  erlp install-release -vr http://repo.erlware.org -d" ++
	" ./tmp/erlware ./sinan.tar.gz~n" ++
	"  erlp help --commands~n" ++
	"  erlp list~n" ++
	"~nFurther help:~n" ++
	"  erlp help --commands # Show all available commands~n" ++
	"  erlp help --command <command> # Show help for the given command~n",

    ?INFO(Msg, []).


error(_Error) ->
    "who knows what happened?~n".

description() ->
    "list packages".

-spec spec() -> get_opts_spec().
spec() ->
    CmdLnTail = "",
    OptionsTail = "",
    OptionSpecs =
	[
      %% {Name,   ShortOpt, LongOpt,        ArgSpec,           HelpMsg}
	 {verbose,    $v,  "verbose",     undefined,   "Verbose output"},
	 {command,    $c,  "command",     string,      "show help for the command supplied"},
	 {commands,   $a,  "commands",    undefined,   "show all commands possible"}
	],
    {OptionSpecs, CmdLnTail, OptionsTail}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
