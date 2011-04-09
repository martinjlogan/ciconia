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
-module(epl_cmd_config_file_path).

%% API
-export([run/2, error/1, spec/0, description/0]).

-include("erlpl.hrl").
-include("eunit.hrl").

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc List installed packages.
%% @spec (RelName, Options) -> ok
%% where
%%  Options = [Option]
%%   Option = {force, bool()} 
%% @end
%%--------------------------------------------------------------------
run(RelName, Options) ->
    RootDir = epl_util:get_option(root_dir, Options, spec(), required),
    CFP = get_config_file_path(RelName, RootDir, Options),
    ?INFO("~p~n", [CFP]).

error(_Error) ->
    "who knows what happened?~n".

description() ->
    "Find a config file path for a particular release".

-spec spec() -> get_opts_spec().
spec() ->
    CmdLnTail = "<pkg_dir>",
    OptionsTail = [{"pkg_dir", "path to the release package to search"}],
    OptionSpecs =
	[
      %% {Name,   ShortOpt, LongOpt,        ArgSpec,           HelpMsg}
	 {verbose,    $v,  "verbose",     undefined,                "Verbose output"},
	 {root_dir,   $d,  "root_dir",      string,         "The root dir where Erlang is installed"},
	 {version,    $n,  "version",       string,      "The release version number to search"}
	],
    {OptionSpecs, CmdLnTail, OptionsTail}.


get_config_file_path(RelName, RootDir, Options) ->
    case epl_util:get_val(version, Options) of
	undefined ->
	    RelVsn = epl_root_dir_util:find_highest_release_vsn(RelName, RootDir),
	    epl_root_dir_util:find_config_file_path(RootDir, RelName, RelVsn);
	RelVsn ->
	    epl_root_dir_util:find_config_file_path(RootDir, RelName, RelVsn)
    end. 

