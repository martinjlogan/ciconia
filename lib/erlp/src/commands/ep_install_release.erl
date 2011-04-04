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
-module(ep_install_release).

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
run(RelName, Options) ->
    install_rel(RelName, Options).

description() ->
    "Install an Erlang release package".

-spec spec() -> get_opts_spec().
spec() ->
    CmdLnTail = "",
    OptionsTail = "",
    OptionSpecs =
	[
      %% {Name,   ShortOpt, LongOpt,        ArgSpec,           HelpMsg}
	 {verbose, $v, "verbose", undefined, "Option for verbose output"},
	 {root_dir,$d, "root_dir", string, "The root dir for the installation"},
	 {version, $n, "version", string, "The rellication version number"},
	 {force, $f, "force", undefined,
	  "Forces the command to run and eliminates all prompts"},
	 {timeout, $t, "timeout", {integer, 60000},
	  "The timeout value for the operation"}
	],
    {OptionSpecs, CmdLnTail, OptionsTail}.

%%%===================================================================
%%% Internal Functions
%%%===================================================================
install_rel(RelName, Options) ->
    RelList = ep_cache:fetch(RelName, release, Options),
    ep_install_util:ep_cache_fetch_ensure(RelName, RelList),
    RelData = select_rel_to_install(RelList, Options),
    RelBinary = fetch_rel_binary(RelData, Options),
    RelPackagePath = ep_install_util:write_out_package(RelData, RelBinary),
    ?DEBUG("pulled down and placed a rel package at ~p~n", [RelPackagePath]),
    epl_install_release_run(RelPackagePath, Options),
    epl_file:remove(filename:dirname(RelPackagePath), [recursive]).

epl_install_release_run(RelPackagePath, Options) ->
    epl_install_release_run(RelPackagePath, Options, 2).

epl_install_release_run(RelPackagePath, Options, 0) ->
	epl_install_release:run(RelPackagePath, Options);
epl_install_release_run(RelPackagePath, Options, Count) ->
    try 
	epl_install_release:run(RelPackagePath, Options)
    catch
	% ReleaseDir is used here because the compressed package 
	% would have been uncompressed and staged by erlpl
	_C:{uex, {_CurrentFunction, _Line,
		  {missing_apps, {ReleaseDir, MissingApps}}, _Msg}} ->
	    ?DEBUG("apps are missing ~p~n", [MissingApps]),
	    install_missing_apps(ReleaseDir, MissingApps, Options),
	    epl_install_release_run(ReleaseDir, force(Options), Count - 1);
	_C:{uex, {_CurrentFunction, _Line,
		  {missing_erts, {ReleaseDir, ErtsVsn}}, _Msg}} ->
	    ?DEBUG("erts is missing ~p~n", [ErtsVsn]),
	    install_missing_erts(ReleaseDir, ErtsVsn, Options),
	    epl_install_release_run(ReleaseDir, force(Options), Count - 1)
    end.

force(Options) ->
    [{force, true}|lists:delete({force, true}, Options)].

install_missing_apps(RelPackagePath, MissingApps, Options) ->
    CleanOptions = [{root_dir, RelPackagePath}|
		    lists:delete(root_dir, lists:delete(version, Options))],
    % TODO move to using ewl_plists map
    ec_plists:map(fun({AppName, AppVsn}) ->
		      ?DEBUG("The release is missing app ~p at version ~p~n" ++
			     "Installing it now~n", [AppName, AppVsn]),
		      ep_install_app:run(atom_to_list(AppName),
					 [{version, AppVsn}|CleanOptions])
	      end, MissingApps).

install_missing_erts(RelPackagePath, ErtsVsn, Options) ->
    CleanOptions = [{root_dir, RelPackagePath}|
		    lists:delete(root_dir, lists:delete(version, Options))],
    % TODO move to using ewl_plists map
    ?DEBUG("The release is missing erts~s. Installing it now~n", [ErtsVsn]),
    ep_install_erts:run(ErtsVsn, CleanOptions).

fetch_rel_binary([], _Options) ->
    throw(?UEX(rel_not_found,
	       release_not_found_msg(),
	       []));
fetch_rel_binary(RelData, Options) ->
    ep_install_util:fetch_binary(RelData, Options).

select_rel_to_install(RelList, Options) ->
    RelVsn = epl_util:get_option(version, Options, spec(), optional),
    select_rel_to_install2(RelVsn, RelList).

select_rel_to_install2(undefined, RelList) ->
    Hd = hd(RelList),
    RelVsn = Hd#package_info.vsn,
    select_rel_to_install_by_vsn(RelVsn, RelList);
select_rel_to_install2(RelVsn, RelList) ->
    select_rel_to_install_by_vsn(RelVsn, RelList).

select_rel_to_install_by_vsn(RelVsn, [RelData|T]) ->
    case RelData#package_info.vsn of
	RelVsn ->
	    RelData;
	_ ->
	    select_rel_to_install_by_vsn(RelVsn, T)
    end;
select_rel_to_install_by_vsn(_RelVsn, []) ->
    throw(?UEX(release_not_found,
	       release_not_found_msg(),
	       [])).

release_not_found_msg() ->
    "The release you were looking for could not be found~n" ++
	"Try updating your package list with update-cache".
    
