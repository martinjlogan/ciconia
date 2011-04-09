%%%-------------------------------------------------------------------
%%% @author Martin Logan <martinjlogan@Macintosh.local>
%%% @copyright (C) 2011, Martin Logan
%%% @doc
%%%   This module prints help information for erlpl.
%%% @end
%%% Created : 20 Mar 2011 by Martin Logan <martinjlogan@Macintosh.local>
%%%-------------------------------------------------------------------
-module(epl_cmd_help).

%% API
-export([
	 print_command_help/1,
	 run/1,
	 spec/0, 
	 description/0
	]).

-include("erlpl.hrl").
-include("eunit.hrl").

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Print help information
%% @spec (Options) -> ok
%% where
%%  Options = [Option]
%%   Option = {force, bool()} 
run(Options) ->
    help_commands(epl_util:get_val(commands, Options), Options).

%% @doc print all the commands in the application.
-spec print_command_help(atom()) -> no_return().
print_command_help(Application) ->
    ?INFO("~nCommand Listing~n" ++
	  "---------------~n~n", []),
    SrcDirStar =
	filename:join([filename:dirname(code:priv_dir(Application)),
		       "src",
		       "*_cmd_*"]),
    CommandModPaths = filelib:wildcard(SrcDirStar),
    print_command_help1(CommandModPaths),
    ?INFO("~n~n", []).

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
help_commands(true, _Options) ->
    print_command_help(erlpl);
help_commands(_False, Options) ->
    help_command(epl_util:get_val(command, Options), Options).


print_command_help1([CmdPath|T]) ->
    Basename = filename:basename(CmdPath),
    [$l,$r,$e,$.|ModNameR] = lists:reverse(Basename),
    ModNameStr = lists:reverse(ModNameR),
    Command = re:replace(ModNameStr, "^[^_]*_cmd_", "", [{return, list}]),
    Module = list_to_atom(ModNameStr),
    ?INFO(" ~s: ~s~n", [Command, Module:description()]),
    print_command_help1(T);
print_command_help1([]) ->
    [].

					   

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


