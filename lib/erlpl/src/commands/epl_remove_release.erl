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
-module(epl_remove_release).

%% API
-export([run/2, spec/0, description/0]).

-include("erlpl.hrl").

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Remove release(s)
%% @spec (ReleaseName, Options) -> ok
%% where
%%  Options = [Option]
%%   Option = {force, bool()} 
%% @end
%%--------------------------------------------------------------------
run(RelName, Options) ->
    ?DEBUG("AppName ~p Options ~p", [RelName, Options]),
    RootDir = epl_util:get_option(root_dir, Options, spec(), required),
    RelVsns = get_rel_vsns(RelName, RootDir, Options),
    ?INFO("Targeting ~p with versions ~p for deletion~n", [RelName, RelVsns]),
    handle_remove_releases(RootDir, RelName, RelVsns, Options).

description() ->
    "remove a release".

-spec spec() -> get_opts_spec().
spec() ->
    CmdLnTail = "<release_name>",
    OptionsTail =
	[{"release_name", "The release name of the release to be removed."}],
    OptionSpecs =
	[
      %% {Name,   ShortOpt, LongOpt,        ArgSpec,               HelpMsg}
	 {verbose, $v, "verbose", undefined, "Verbose output"},
	 {root_dir, $d, "root_dir", string,
	  "The root dir where Erlang code is installed"},
	 {version, $n, "version", string, "Release version number"},
	 {force, $f,  "force",        {atom, false},
	  "Forces the command to run and eliminates all prompts"}
	],
    {OptionSpecs, CmdLnTail, OptionsTail}.


%%%---------------------------------------------------------
%%% Internal Functions
%%%---------------------------------------------------------

handle_remove_releases(RootDir, RelName, [RelVsn|T], Options) ->
    NonSharedAppSpecs = find_non_shared_app_specs(RootDir, RelName, RelVsn),
    Prompt = io_lib:fwrite("Do you want to remove the release for release ~p~nand the associated app(s)~n~p~n(y/n)?",
			   [{RelName, RelVsn}, NonSharedAppSpecs]), 
    RespSet = ["y", "n"],
    SuccessSet = ["y"],
    %% XXX TODO handle removing bin files if the release removed is the latest according to the db.
    Fun = fun() ->
		  remove_apps(RootDir, NonSharedAppSpecs),
		  remove_release(RootDir, RelName, RelVsn)
	  end,
    epl_util:force_or_prompt_for_an_action(Prompt, RespSet, SuccessSet, Fun, Options),
    handle_remove_releases(RootDir, RelName, T, Options);
handle_remove_releases(_RootDir, _RelName, [], _Options) ->
    ok.

remove_release(RootDir, RelName, RelVsn) ->
    %% XXX TODO how to handle the executable files in bin. Perhaps diff against those in the rel dir and remove if same? 
    ewl_file:delete_dir(epl_installed_paths:release_dir(RootDir, RelName, RelVsn)),
    epl_db:remove_release(RelName, RelVsn).

remove_apps(RootDir, AppSpecs) ->
    try
	lists:foreach(fun({AppName, AppVsn}) ->
			      ewl_file:delete_dir(epl_installed_paths:app_dir(RootDir, atom_to_list(AppName), AppVsn)),
			      epl_db:remove_app(AppName, AppVsn)
		      end,
		      AppSpecs)
    catch
	_C:E ->
	    Msg = "Failed to delete one of the applications.~n~p~nCheck to see that you have proper permissions~n",
	    throw(?UEX({failed_to_remove_app, E}, Msg, [AppSpecs]))
    end.
    
get_rel_vsns(RelName, RootDir, Options) ->
    case epl_util:get_val(version, Options) of
	undefined ->
	    epl_root_dir_util:list_release_vsns(RootDir, RelName);
	Vsn ->
	    [Vsn]
    end.
		

%%--------------------------------------------------------------------
%% @private
%% @doc return a list of specs for apps from a particular release that are not shared with any other release.
%% @end
%%--------------------------------------------------------------------
find_non_shared_app_specs(RootDir, RelName, RelVsn) ->
    {ok, TargetSpecs} = fetch_app_specs(RootDir, RelName, RelVsn),
    ReleaseNames  = epl_root_dir_util:list_release_names(RootDir),
    ReleaseTuples = lists:map(fun(ReleaseName) when ReleaseName == RelName ->
				      ReleaseVersions = epl_root_dir_util:list_release_vsns(RootDir, ReleaseName),
				      {ReleaseName, lists:delete(RelVsn, ReleaseVersions)};
				 (ReleaseName) ->
				      ReleaseVersions = epl_root_dir_util:list_release_vsns(RootDir, ReleaseName),
				      {ReleaseName, ReleaseVersions}
			      end, ReleaseNames),
    FlatSpecs = fetch_flat_list_app_specs(RootDir, ReleaseTuples),
    TargetSpecs -- FlatSpecs.
    
fetch_flat_list_app_specs(RootDir, ReleaseTuples) ->
    lists:flatten(fetch_flat_list_app_specs2(RootDir, ReleaseTuples)).

fetch_flat_list_app_specs2(RootDir, [{RelName, RelVsns}|ReleaseTuples]) ->
    lists:map(fun(RelVsn) ->
		      case fetch_app_specs(RootDir, RelName, RelVsn) of
			  {ok, AppSpecs} -> AppSpecs;
			  _              -> []
		      end
	      end, RelVsns) ++ fetch_flat_list_app_specs2(RootDir, ReleaseTuples);
fetch_flat_list_app_specs2(_, []) ->
    [].

fetch_app_specs(RootDir, RelName, RelVsn) when is_atom(RelName) ->
    fetch_app_specs(RootDir, atom_to_list(RelName), RelVsn);
fetch_app_specs(RootDir, RelName, RelVsn) ->
    RelFilePath = epl_installed_paths:rel_file_path(RootDir, RelName, RelVsn),
    AppSpecs = epl_otp_metadata_lib:consult_rel_file(app_specs, RelFilePath),
    {ok, [{element(1, AS), element(2, AS)} || AS <- AppSpecs]}.

