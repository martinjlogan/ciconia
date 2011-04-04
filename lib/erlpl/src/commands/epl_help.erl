%%%-------------------------------------------------------------------
%%% @author Martin Logan <martinjlogan@Macintosh.local>
%%% @copyright (C) 2011, Martin Logan
%%% @doc
%%%   This module prints help information for erlpl.
%%% @end
%%% Created : 20 Mar 2011 by Martin Logan <martinjlogan@Macintosh.local>
%%%-------------------------------------------------------------------
-module(epl_help).

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
	  "  rollback-release~n" ++
	  "  remove-release~n" ++
	  "  dump-db~n" ++
	  "  manage-root-dir~n" ++
	  "  config-file-path~n" ++
	  "  help~n~n", []);
help_commands(_False, Options) ->
    help_command(epl_util:get_val(command, Options), Options).

help_command(undefined, _Options) ->
    help();
help_command(Command, Options) ->
    epl_cmdln_lib:print_usage_for_command(Command, Options).

help() ->
    Msg =
	"erlpl is serious package management for Erlang/OTP This is a~n" ++
	"basic help message containing pointers to more information.~n" ++
	"~nUsage:~n" ++
	"  erlpl help --commands~n" ++
	"  erlpl version~n" ++
	"  erlpl <command> [options] [arguments]~n" ++
	"~nExamples:~n" ++
	"  erlpl install-release -v -d ./tmp/erlware ./sinan.tar.gz~n" ++
	"  erlpl help --commands~n" ++
	"  erlpl list~n" ++
	"~nFurther help:~n" ++
	"  erlpl help --commands # Show all available commands~n" ++
	"  erlpl help --command <command> # Show help for the given command~n",

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
      %% {Name,   ShortOpt, LongOpt,        ArgSpec,   HelpMsg}
	 {verbose,    $v,  "verbose",     undefined,   "Verbose output"},
	 {command,    $c,  "command",     string,      "show help for the command supplied"},
	 {commands,   $a,  "commands",    undefined,   "show all commands possible"}
	],
    {OptionSpecs, CmdLnTail, OptionsTail}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
