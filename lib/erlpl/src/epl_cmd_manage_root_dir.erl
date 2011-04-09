%%% @author Martin Logan <martinjlogan@Macintosh.local>
%%% @copyright (C) 2011, Martin Logan
%%% @doc
%%%  Point this at a root dir to bring it under erlpl management.
%%% @end
%%% Created : 20 Mar 2011 by Martin Logan <martinjlogan@Macintosh.local>
-module(epl_cmd_manage_root_dir).



%% API
-export([run/2, spec/0, description/0]).

-include("erlpl.hrl").
-include("eunit.hrl").

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Point this at a root dir to bring it under erlpl management.
-spec run(string(), option_list()) -> ok.
run(RootDir, Options) ->
    LibDir = epl_installed_paths:lib_dir(RootDir),
    ReleasesDir = epl_installed_paths:releases_dir(RootDir),
    validate_lib_dir(LibDir),
    epl_installed_info:add_managed_root_dir([{root_dir, RootDir}|Options]),
    add_package_info(RootDir, LibDir, ReleasesDir,
		     [{root_dir, RootDir}|Options]).

description() ->
    "list packages".

-spec spec() -> get_opts_spec().
spec() ->
    CmdLnTail = "<root_dir>",
    OptionsTail = [{"root_dir", "path to the root dir to be managed."}],
    OptionSpecs =
	[
      %% {Name,   ShortOpt, LongOpt,        ArgSpec,           HelpMsg}
	 {verbose,    $v,  "verbose",       undefined,      "Verbose output"}
	],
    {OptionSpecs, CmdLnTail, OptionsTail}.

%%%===================================================================
%%% Internal Functions
%%%===================================================================


add_package_info(RootDir, LibDir, ReleasesDir, Options) ->
    SubDirs =
	lists:concat([filelib:wildcard(RootDir ++ "/*"),
		      filelib:wildcard(LibDir ++ "/*"),
		      filelib:wildcard(ReleasesDir ++ "/*")]),
    add_package_info(SubDirs, Options).

%% XXX TODO make this parallel with pmap
add_package_info([SubDir|T], Options) ->
    try
	?INFO(". ", []),
	epl_installed_info:write(SubDir, Options)
    catch
	_C:_E ->
	    ?DEBUG("skipping non-package ~p~n", [SubDir]),
	    ok
    end,
    add_package_info(T, Options);
add_package_info([], _Options) ->
    ?INFO(".~n", []),
    ok.

validate_lib_dir(LibDir) ->
    case filelib:is_dir(LibDir) of
	true ->
	    ok;
	false ->
	    throw(?UEX(bad_root_dir,
		       "A valid root dir must have a lib directory",
		       []))
    end.

