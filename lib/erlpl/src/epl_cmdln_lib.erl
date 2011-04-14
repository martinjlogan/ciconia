%%%-------------------------------------------------------------------
%%% @author Martin Logan <martinjlogan@Macintosh.local>
%%% @copyright (C) 2010, Martin Logan
%%% @doc
%%%  This is the entry point for erlp when used from the commandline.
%%% @end
%%% Created : 20 Jun 2010 by Martin Logan <martinjlogan@Macintosh.local>
%%%-------------------------------------------------------------------
-module(epl_cmdln_lib).

%% API
-export([
	 apply_cmdln/1,
	 print_usage_for_command/2
	]).

-include("erlpl.hrl").
-include("eunit.hrl").

%%%===================================================================
%%% API
%%%===================================================================

%% @doc evaluate commandline from init:get_plain_args and call the
%%      appropriate function in the module supplied by Module. The
%%      arguments it takes are the program name, the location that
%%      erlp is installed, and the prefix of the modules to call into.
-spec apply_cmdln(list()) -> ok.
apply_cmdln([ProgName, ErlpRootDir, MetaDir, Prefix]) ->
    InitialOptions = [{erlp_root_dir, atom_to_list(ErlpRootDir)},
		      {meta_dir, atom_to_list(MetaDir)},
		      {prog_name, atom_to_list(ProgName)},
		      {prefix, atom_to_list(Prefix)}],
    try
	{Module, Function, Args, Options} =
	    parse_args(init:get_plain_arguments(), InitialOptions),
	set_verbose(Options),
	apply_cmdln(Module, Function, Args, Options)
    catch
	_:{uex, {_CurrentFunction, _Line, _Error, _Msg}} = E ->
	    print_execution_failure_err_msg(epl_cmdln_lib, [],
					    InitialOptions, E),
	    shutdown(1);
	_:E ->
	    ?INFO("Command not found~n", []),
	    help(InitialOptions),
	    ?WARN("ERROR: ~p~n~nStackTrace:~p~n",
		  [E, erlang:get_stacktrace()]),
	    shutdown(1)
    end.

%% @doc print usage for a command to stdout.
-spec print_usage_for_command(string(), option_list()) -> ok.
print_usage_for_command(Command, Options) ->
    Module = module_base_to_module(
	       translate_dash_to_underscore(Command),
	       Options),
    print_usage(Module, Options).



%%%===================================================================
%%% Internal functions
%%%===================================================================

apply_cmdln(Module, Function, Args, Options) ->
    try
	%lock(Options),
	FullArgs = Args ++ [Options],
	apply(Module, Function, FullArgs),
	%unlock(Options),
	shutdown(0)
    catch
	_:{uex, {_CurrentFunction, _Line,
		 failed_to_lock_package_db, _Msg}} = E ->
	    print_execution_failure_err_msg(Module, Args, Options, E),
	    shutdown(1);
	_:E ->
	    %unlock(Options),
	    print_execution_failure_err_msg(Module, Args, Options, E),
	    shutdown(1)
    end.

parse_args([], InitialOptions) ->
    parse_args(["help"], InitialOptions);
parse_args([Command|PlainArgs], InitialOptions) ->
    Module = module_base_to_module(translate_dash_to_underscore(Command),
				   InitialOptions),
    {OptionSpecs, _, _} = get_spec(Module, InitialOptions),
    {Options, Args} = getopt_parse(OptionSpecs, PlainArgs),
    %% Format the repos option
    %% Args come before the options list when we apply. 
    {Module, run, Args, InitialOptions ++ format_repos_option(Options)}.

get_spec(Module, Options) ->
    try
	apply(Module, spec, [])
    catch
	_C:_E ->
	    Prefix = epl_util:get_option(prefix, Options),
	    Command = remove_prefix(Prefix, Module),
	    throw(?UEX(unknown_function,
		       "The command you are trying to execute ~p does not~n" ++
		       "exist and no help can be given. Please run~n" ++
		       "help commands for more information on all commands",
		       [Command]))
    end.

getopt_parse(OptionSpecs, PlainArgs) ->
    case getopt:parse(OptionSpecs, PlainArgs) of
	{ok, {Options, Args}} ->
	    {Options, Args};
	{error,{invalid_option, Option} = Error} ->
	    throw(?UEX(Error,
		       "The commandline option ~p is not~n" ++
		       "valid for this function~n",
		       [Option]))
    end.

format_repos_option(Options) ->
    case epl_util:get_val(repos, Options) of
	undefined ->
	    Options;
	Repos_ -> 
	    Repos = string:tokens(Repos_, ","),
	    [{repos, Repos}|lists:keydelete(repos, 1, Options)]
    end.

module_base_to_module(ModuleBase, Options) ->
    Prefix = epl_util:get_option(prefix, Options),
    list_to_atom(Prefix ++ "_" ++ ModuleBase).

print_execution_failure_err_msg(Module, Args, Options, Exception) ->
    ?INFO("~n**Failure: see below.~n", []),
    case Exception of
	{uex, {CurrentFunction, Line, ActualEx, UsrMsg}} ->
	    ?INFO("~n**Hint**~n~s~n~n", [UsrMsg]),
	    ?DEBUG("~n~n***Debug Info***~n" ++
		   "Exception processed: in ~p ~p at ~p with~n" ++
		   "~p~n~nStackTrace:~p~n",
		   [CurrentFunction, Args, Line, ActualEx,
		    erlang:get_stacktrace()]);
	{ex, {CurrentFunction, Line, ActualEx}} ->
	    ?DEBUG("~n***Debug Info***~n" ++
		   "Exception processed: in ~p ~p at ~p with~n" ++
		   "~p~n~nStackTrace:~p~n",
		   [CurrentFunction, Args, Line, ActualEx,
		    erlang:get_stacktrace()]);
	Exception ->
	    ?DEBUG("~n***Debug Info***~n" ++
		   "Exception processed: ~p~n~nStackTrace:~p~n",
		   [Exception, erlang:get_stacktrace()])
    end,
    print_usage(Module, Options),
    suggest_log_level().

suggest_log_level() ->
    case os:getenv("ERLP_LOG_LEVEL") of
	Level when Level /= "1" ->
	    ?INFO("To know more try using the -v option for more verbose output~n", []);
	_ ->
	    ok
    end.


translate_dash_to_underscore(String) ->
    re:replace(String, "-", "_", [{return, list}, global]).

remove_prefix(Prefix, CommandName) when is_atom(CommandName) ->
    remove_prefix(Prefix, atom_to_list(CommandName));
remove_prefix(Prefix, CommandName) ->
    try
	Ret = re:replace(CommandName, Prefix ++ "_", "",
			 [{return, list}, global]),
	false = (CommandName == Ret),
	Ret
    catch
	_C:_E ->
	    case strip_prefix(CommandName) of
		error ->
		    throw(?UEX(bad_command_name,
			       "This is an internal error - nothing you" ++
			       " can do. Something" ++
			       " is wrong with the command prefix ~p ~p.~n",
			       [Prefix, CommandName]));
		Command ->
		    Command
	    end
    end.

strip_prefix([$_|T]) -> T;
strip_prefix([_H|T]) -> strip_prefix(T);
strip_prefix([])     -> error.
    
set_verbose(Options) ->
    case catch epl_util:get_option(verbose, Options) of
	true -> os:putenv("ERLP_LOG_LEVEL", "1");
	_    -> ok
    end.

shutdown(ExitNum) ->
    % XXX This is due to halt cutting off io before the user sees it.
    % Need another solution ideally. 
    timer:sleep(500),
    halt(ExitNum).

%% @doc Lock the DB
-spec lock(list()) -> ok.
lock(Options) ->
    % Seed for sleeping that may be done later.
    {A, B, C} = erlang:now(),
    random:seed(A, B, C),
    LockFileName = lock_file_name(Options),
    lock(10, LockFileName, Options).

lock(0, LockFileName, _Options) ->
    Msg = "Failed to lock the db. You may have an old lock file in place~n" ++ 
	"or another user is running a long running application.~n" ++
	"The problem may be fixed by deleting:~n~s~n",
    throw(?UEX(failed_to_lock_package_db, Msg, [LockFileName]));
lock(Count, LockFileName, Options) ->
    case filelib:is_file(LockFileName) of
	false ->
	    Contents = term_to_binary(now()),
	    ewl_file:mkdir_p(filename:dirname(LockFileName)),
	    epl_file:write(LockFileName, Contents),
	    case epl_file:read(LockFileName) of
		Contents ->
		    ok;
		_Miss ->
		    SleepTime = random:uniform(99) * 20,
		    ?DEBUG("sleeping waiting for lock file for ~p milliseconds~n", [SleepTime]), 
		    timer:sleep(SleepTime),
		    lock(Count - 1, LockFileName, Options)
	    end;
	true ->
	    SleepTime = random:uniform(99) * 20,
	    ?DEBUG("sleeping waiting for lock file for ~p milliseconds~n",
		   [SleepTime]), 
	    timer:sleep(SleepTime),
	    lock(Count - 1, LockFileName, Options)
    end.

%% @doc Unlock the DB
-spec unlock(list()) -> ok.
unlock(Options) ->
    ?DEBUG("deleting lock file ~p~n", [lock_file_name(Options)]),
    file:delete(lock_file_name(Options)).

lock_file_name(Options) ->
    MetaDir = epl_util:get_option(meta_dir, Options),
    ewl_file:mkdir_p(MetaDir),
    filename:join(MetaDir, "erlpl_lock").
    
help(Options) ->
    parse_args(["help"], Options).

print_usage(Module, Options) ->
    ?INFO("~n", []),
    {OptionSpecList, CmdLnTail, OptionsTail} = apply(Module, spec, []),
    ProgName = epl_util:get_option(prog_name, Options),
    Prefix = epl_util:get_option(prefix, Options),
    ProgAndFunction = lists:concat([ProgName, " ", remove_prefix(Prefix,
						     atom_to_list(Module))]),
    getopt:usage(OptionSpecList, ProgAndFunction, CmdLnTail, OptionsTail).

%%%===================================================================
%%% Test functions
%%%===================================================================
