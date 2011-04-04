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
-module(epl_rollback_release).

%% API
-export([run/2, error/1, spec/0, description/0]).

-include("erlpl.hrl").

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Rollback a release for a previously installed one.
%% @spec (RelName, Options) -> ok
%% where
%%  Options = [Option]
%%   Option = {force, bool()} | {lib_dirs, [string()]}
%% @end
%%--------------------------------------------------------------------
run(RelName, Options) ->
    handle_rollback(RelName, Options).
    
error(_Error) ->
    "no clue why this failed!~n".

description() ->
    "rollback a release package".

-spec spec() -> get_opts_spec().
spec() ->
    CmdLnTail = "<rel_name>",
    OptionsTail = [{"rel_name", "name of release package to" ++
		    " be targted for release rollback"}],
    OptionSpecs =
	[
      %% {Name,   ShortOpt, LongOpt,        ArgSpec,               HelpMsg}
	 {verbose, $v, "verbose", undefined, "Verbose output"},
	 {root_dir, $d, "root_dir", string, "The root dir for the install"},
	 {version, $n, "version", string, "Version"},
	 {force, $f, "force", undefined, "Forces the command to run and eliminates all prompts"}
	],
    {OptionSpecs, CmdLnTail, OptionsTail}.


%%%===================================================================
%%% Internal functions
%%%===================================================================

handle_rollback(RelName, Options) ->
    RootDir = epl_util:get_option(root_dir, Options, spec(), required),
    RelInfo = info_for_rel_vsn(RelName, RootDir, Options),
    RelVsn = RelInfo#package_info.vsn,
    ?INFO("Starting rollback to ~p at version ~p~n", [RelName, RelVsn]),
    BinFiles = stage_bin_files(RelInfo),
    ?DEBUG("bin files to reinstall are ~p~n", [BinFiles]),
    epl_install_driver:install_executables(RootDir, BinFiles),
    delete_staged_bin_files(BinFiles).

stage_bin_files(RelInfo) ->
    TmpDir = ewl_file:make_tmp_dir(),
    Executables = exeutables(RelInfo),
    lists:map(fun({FileName, Contents}) ->
		      WrittenBinFilePath = filename:join(TmpDir, FileName),
		      epl_file:write(WrittenBinFilePath, Contents),
		      WrittenBinFilePath
	      end, Executables).

delete_staged_bin_files(BinFiles) ->
    epl_file:remove(filename:dirname(hd(BinFiles)), [recursive]).

exeutables(RelInfo) ->
    BinFiles = epl_util:get_val(bin_files, RelInfo#package_info.meta),
    case BinFiles of
	undefined ->
	    throw(?UEX(rollback_failed,
		       "There are no executable files associated with ~s~n" ++
		       "so rolling back has no meaning,",
		       [RelInfo#package_info.name]));
	BinFiles ->
	    BinFiles
    end.
	
info_for_rel_vsn(RelName, RootDir, Options) ->
    RelVsn = epl_util:get_option(version, Options, spec(), optional),
    info_for_rel_vsn(RelName, RelVsn, RootDir, Options).

info_for_rel_vsn(RelName, undefined, _RootDir, Options) ->
    Releases = epl_installed_info:releases_under_root(RelName, Options),
    find_second_highest_release(Releases);
info_for_rel_vsn(RelName, RelVsn, _RootDir, Options) ->
    try
	epl_installed_info:release_under_root(RelName, RelVsn, Options)
    catch
	_C:_E ->
	    rollback_exception(RelName)
    end.

find_second_highest_release([]) ->
    throw(?UEX(rollback_failed,
	       "The release you are trying to rollback is not managed~n" ++
	       "please run the manage_root_dir command on the root dir~n",
	       []));
find_second_highest_release([#package_info{name = RelName}]) ->
    rollback_exception(RelName);
find_second_highest_release(PackageInfoList) ->
    SortedPackageInfoList =
	lists:sort(fun(A, B) ->
			   ec_string:compare_versions(A#package_info.vsn,
						      B#package_info.vsn)
		   end,
		   PackageInfoList),
    [_, Info|_] = SortedPackageInfoList,
    Info.
	    
rollback_exception(RelName) ->
    throw(?UEX(rollback_failed,
	       "There was no previous version of ~s to rollback to",
	       [RelName])).

%%%===================================================================
%%% Test Functions
%%%===================================================================

-ifndef(NOTEST).
-include_lib("eunit/include/eunit.hrl").

find_highest_release_test() ->
    PkgInfoList = [#package_info{vsn = "1.2.3"},
		   #package_info{vsn = "1.3.0"},
		   #package_info{vsn = "1.2.4"}],

    ?assertMatch(#package_info{vsn = "1.2.4"},
		find_second_highest_release(PkgInfoList)).

-endif.
