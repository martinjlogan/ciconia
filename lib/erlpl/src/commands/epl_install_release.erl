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
-module(epl_install_release).

%% API
-export([run/2, spec/0, description/0]).

-include("erlpl.hrl").

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Install a release package. 
%% @spec (ReleaseDir, Options) -> ok
%% where
%%  Options = [Option]
%%   Option = {force, bool()} | {lib_dirs, [string()]}
-spec run(ReleaseDir::string(), Options::list()) -> ok.
run(RawReleaseDir, Options) ->
    ReleaseDir = epl_util:unpack_to_tmp_if_archive(RawReleaseDir),
    ?DEBUG("Unpacked release to ~p~n", [ReleaseDir]),
    Res = 
	case epl_validation:release_validation(ReleaseDir) of
	    {error, Reason} ->
		Msg = "Validation for ~s failed with ~p.~n" ++
		    "Please verify that you have a well formed~n" ++
		    "OTP Release Package~n",
		Vars =  [ReleaseDir, Reason],
		throw(?UEX({validation_of_release_failed, Reason}, Msg, Vars));
	    true -> 
		epl_installed_info:add_managed_root_dir(Options),
		handle_install_release(ReleaseDir, Options)
	end,
    epl_file:remove(ReleaseDir, [recursive]),
    Res.
	
description() ->
    "install a release package".

-spec spec() -> get_opts_spec().
spec() ->
    CmdLnTail = "<pkg_dir>",
    OptionsTail = [{"pkg_dir", "path to the release package to be installed"}],
    OptionSpecs =
	[
      %% {Name,   ShortOpt, LongOpt,        ArgSpec,               HelpMsg}
	 {verbose, $v, "verbose", undefined, "Verbose output"},
	 {root_dir, $d, "root_dir", string, "The root dir for the install"},
	 {erts_dir, $e, "erts_dir", string, "The erts dir for the install"},
	 {force, $f, "force", undefined,
	  "Forces the command to run and eliminates all prompts"}
	],
    {OptionSpecs, CmdLnTail, OptionsTail}.


%%%===================================================================
%%% Internal functions
%%%===================================================================

handle_install_release(ReleaseDir, Options) ->
    RootDir  = epl_util:get_option(root_dir, Options, spec(), required),
    RelFile = fetch_rel_file(ReleaseDir),
    [RelName, RelVsn] =
	epl_otp_metadata_lib:consult_rel_file([name, vsn], RelFile),
    InstalledRelFileDir = epl_installed_paths:release_dir(RootDir, RelName,
							  RelVsn),
    LibDirs = [epl_installed_paths:lib_dir(ReleaseDir)],
    Fun = fun() ->
		  stage_missing_apps(ReleaseDir, Options),
		  do_install_release(ReleaseDir, RelFile, LibDirs,
				     RootDir, Options),
		  install_executables(ReleaseDir, RootDir),
		  install_erts(RelFile, ReleaseDir, RootDir, Options)
	  end,
    %% TODO add in db support for rollback and such
    prompt_or_force_release_install(InstalledRelFileDir, RelName,
				    RelVsn, Fun, Options),
    
    case is_config_modified(RootDir, RelName, Options) of
	false ->
	    ?DEBUG("Config was not modified - no need for diff~n", []),
	    ok;
	true ->
	    prompt_or_force_diff_config(RootDir, RelName, RelVsn, Options)
    end.
    
prompt_or_force_diff_config(RootDir, RelName, RelVsn, Options) ->
    Records = lists:sort(fun(#package_info{timestamp = T1},
			     #package_info{timestamp = T2}) -> T1 > T2 end,
			 epl_installed_info:releases(RelName, Options)),
    [#package_info{vsn = RelVsn}, #package_info{vsn = OldRelVsn}|_] = Records,
    case diff_config(RootDir, RelName, RelVsn, OldRelVsn) of
	[] ->
	    ok;
	Diffs ->
	    lists:foreach(
	      fun({Rel1ConfigFilePath, Rel2ConfigFilePath, Diff}) ->
		      Fun = fun() ->
				    ?DEBUG("replacing ~p with ~p~n",
					   [Rel2ConfigFilePath,
					    Rel1ConfigFilePath]),
				    epl_file:copy(Rel2ConfigFilePath,
						  Rel1ConfigFilePath)
			    end,
		      RawPrompt = "A config file for release ~s-~s appears" ++
			  " to have been modified and differs~n" ++
			  "from the config at version ~s~nDiff:~n~p~n" ++
			  "Would you like keep the current config or" ++
			  " upgrade to the latest version?~n" ++
			  "Enter \"k\" to keep the current or \"u\"" ++
			  " to upgrade your config~n",
		      Prompt = lists:flatten(io_lib:fwrite(RawPrompt,
							   [RelName, RelVsn,
							    OldRelVsn, Diff])),
		      RespSet = ["k", "u"],
		      SuccessSet = ["k"],
		      epl_util:force_or_prompt_for_an_action(Prompt, RespSet,
							     SuccessSet, Fun,
							     Options)
	      end, Diffs)
    end.

is_config_modified(RootDir, RelName, Options) ->
    Records = epl_installed_info:releases(RelName, Options),
    Releases = lists:sort(fun(#package_info{timestamp = T1},
			      #package_info{timestamp = T2}) ->
				  T1 > T2 end, Records),
    case Releases of
	[#package_info{vsn = RelVsn},
	 #package_info{vsn = OldRelVsn, meta = Meta}|_] ->
	    StoredConfigChecksums = epl_util:get_val(config_terms, Meta),
	    ?DEBUG("found second oldest package ~p ~p and newest was ~p~n",
		   [RelName, OldRelVsn, RelVsn]),
	    is_config_modified(RootDir, RelName, OldRelVsn,
			       StoredConfigChecksums);
	_NotFound ->
	    ?DEBUG("Could not find second oldest package to discover" ++
		   " modified config for release ~p~n", [RelName]),
	    false
    end.
	    
is_config_modified(RootDir, RelName, RelVsn, StoredConfigChecksums) ->
    case epl_root_dir_util:find_config_file_path(RootDir, RelName, RelVsn) of
	[] ->
	    % If no config in project then no
	    ?DEBUG("No config files found in ~p ~p - skipping diff~n",
		   [RelName, RelVsn]),
	    false;
	[ConfigFilePath] ->
	    Checksum = ewl_file:md5_checksum(epl_file:read(ConfigFilePath)),
	    ConfigFileName = filename:basename(ConfigFilePath),
	    % compare checksums
	    epl_util:do_until(
	      fun({ConfigFileName1, Checksum1}) ->
		      ?DEBUG("checking checksums for ~p with ~p againt ~p~n",
			     [ConfigFilePath, Checksum, Checksum1]),
		      (not (Checksum =:= Checksum1))
			  andalso
		      ConfigFileName =:= ConfigFileName1
	      end, true, StoredConfigChecksums);
	_Multiple ->
	    % XXX TODO this can be made more robust - can track multiple
	    % If there are multiple configs we just say no becuse there
	    % is no way to tell which is used
	    ?DEBUG("Multiple config files found in ~p ~p - skipping diff~n",
		   [RelName, RelVsn]),
	    false
    end.

				 
prompt_or_force_release_install(InstalledRelFileDir, RelName, RelVsn,
				Fun, Options) ->
    case catch epl_local_ts:fetch_releases(RelName, Options) of
	Releases when is_list(Releases) andalso length(Releases) > 0 -> 
	    try
		prompt_or_force_release_install2(InstalledRelFileDir, RelName,
						 RelVsn, Fun, Options)
	    catch
		_C:E ->
		    #package_info{vsn = OldVsn} = hd(Releases),
		    ?DEBUG("Release install for ~p ~p failed with ~p~n." ++
			   "Rolling back to last good version ~p~n~p~n",
			  [RelName,RelVsn,E,OldVsn,erlang:get_stacktrace()]),
		    SelectOptions =
			lists:foldl(
			  fun(Key, Acc) ->
				  [{Key,
				    epl_util:get_option(Key, Options)}|Acc]
			  end, [{version, OldVsn}], [root_dir, meta_dir, erlp_root_dir]),
		    epl_rollback_release:run(RelName, SelectOptions),
		    throw(E)
	    end;
	_Error ->
	    prompt_or_force_release_install2(InstalledRelFileDir, RelName,
					     RelVsn, Fun, Options)
    end.
	    
prompt_or_force_release_install2(InstalledRelFileDir, RelName,
				 RelVsn, Fun, Options) ->
    case filelib:is_dir(InstalledRelFileDir) of
	true ->
	    Prompt = io_lib:fwrite("Do you want to overwrite the release;" ++
				   " ~s-~s (y/n)?", [RelName, RelVsn]), 
	    RespSet = ["y", "n"],
	    SuccessSet = ["y"],
	    epl_util:force_or_prompt_for_an_action(Prompt, RespSet,
						   SuccessSet, Fun, Options);
	false ->
	    Fun()
    end.

install_erts(RelFile, ReleaseDir, RootDir, Options) ->
    ErtsVsn = epl_otp_metadata_lib:consult_rel_file(erts_vsn, RelFile),
    InstalledErtsDir = epl_installed_paths:erts_dir(RootDir, ErtsVsn),
    install_erts_if_needed(filelib:is_dir(InstalledErtsDir), RelFile,
			   ReleaseDir, ErtsVsn, RootDir, Options).

install_erts_if_needed(false, RelFile, ReleaseDir, ErtsVsn, RootDir, Options) ->
    ErtsDir = erts_dir(RelFile, ReleaseDir, Options),
    case ErtsDir of
	undefined ->
	    Msg = "Failed because erts-~s is missing from ~p.~n" ++
	"This is required for installation. if the package is not~n" ++
	"found in a repo you can install the corresponding erlang~n" ++
	"version from source via erlang.org and install the erts~n" ++
	"package provided within. If the erts package is on your~n" ++
	"system then you can use the 'erts_dir' option to specify it.~n" ++
	"NOTE - there are two erts packages in an erlang install one~n" ++ 
	"in 'lib' and one beside it. You want the one beside lib~n" ++
	"because the.~n" ++
	"one under the lib dir is an erlang application and not~n" ++
	"the erlang runtime system itself.",
	    Vars = [ErtsVsn, filename:basename(ReleaseDir)],
	    throw(?UEX({missing_erts, {ReleaseDir, ErtsVsn}}, Msg, Vars));
	ErtsDir ->
	    TmpErtsDir = epl_util:unpack_to_tmp_if_archive(ErtsDir),
	    epl_installed_info:write(TmpErtsDir, Options),
	    epl_install_driver_install_erts(RelFile, RootDir, TmpErtsDir),
	    epl_file:remove(TmpErtsDir, [recursive])
    end;
install_erts_if_needed(true, _RelFile, _ReleaseDir,
		       _ErtsVsn, _RootDir, _Options) ->
    ok.

epl_install_driver_install_erts(RelFile, RootDir, ErtsDir) ->
    try
	epl_install_driver:install_erts(RelFile, RootDir, ErtsDir)
    catch
	_C:{ex, {_CurrentFunction, _Line, {mismatched_erts_vsns,
					   InstalledErtsDir,
					   InstalledErtsDir2}}} ->
	    Msg = "The ERTS version specefied by the rel file is ~s~n" ++
		"It does not match the ERTS version provided ~s~n" ++
		"This means you are trying to install a release that~n" ++
		"requires one version of ERTS with another version.~n" ++
		"This will not work. Please find the right ERTS package~n" ++
		"for this install.",
	    throw(?UEX({mismatched_erts_vsns, InstalledErtsDir,
			InstalledErtsDir2},
		       Msg,
		       [InstalledErtsDir, InstalledErtsDir2]))
    end.
    
erts_dir(RelFile, ReleaseDir, Options) ->
    ErtsVsn = epl_otp_metadata_lib:consult_rel_file(erts_vsn, RelFile),
    case epl_util:get_val(erts_dir, Options) of
	undefined ->
	    try epl_installed_info:erts(ErtsVsn, Options) of
		ErtsRecord ->  
		    ErtsRecord#package_info.path
	    catch
		_C:_E ->
		    erts_dir_in_release(ErtsVsn, ReleaseDir)
	    end;
	ErtsDir ->
	    ErtsDir
    end.

erts_dir_in_release(ErtsVsn, ReleaseDir) ->
    ErtsDir = epl_installed_paths:erts_dir(ReleaseDir, ErtsVsn),
    IsDir   = filelib:is_dir(ErtsDir),
    case IsDir of
	true            -> ErtsDir;
	false           -> undefined
    end.

fetch_rel_file(ReleaseDir) ->
    try
	[RelFile] = filelib:wildcard(filename:join([epl_installed_paths:releases_dir(ReleaseDir), "*","*.rel"])),
	RelFile
    catch
	_C:_E ->
	    throw(?UEX(release_validation_failed,
		       "more than one .rel file present in ~p~n",
		       [ReleaseDir]))
    end.


install_executables(ReleaseDir, RootDir) ->
    BinDir = filename:join(ReleaseDir, "bin"),
    Executables = filelib:wildcard(filename:join(BinDir, "*")),
    epl_install_driver:install_executables(RootDir, Executables).

%% TODO we need to pass over erts dir conditionally
do_install_release(ReleaseDir, RelFile, LibDirs, RootDir, Options) ->
    RelFileDir = filename:dirname(RelFile),
    back_up_bin_in_release(ReleaseDir, RelFileDir),
    RelFile = epl_util:fetch_rel_file_from_release_package(RelFileDir),
    AppSpecs = epl_otp_metadata_lib:consult_rel_file(app_specs, RelFile),
    AltConfigFilePath = undefined,
    ExecutableFiles = undefined,
    ErtsDir = undefined,
    %% XXX TODO a rollback on failure of a release install would be good here
    write_info_to_all_apps(ReleaseDir, AppSpecs, Options),
    PIP = epl_installed_paths:package_info_path(RelFileDir),
    epl_installed_info:write_to(ReleaseDir,
				PIP,
				Options),
    ?INFO(".~n", []),
    epl_install_driver:install_release(RelFileDir, AltConfigFilePath,
				       ExecutableFiles, LibDirs, ErtsDir,
				       RootDir).

write_info_to_all_apps(ReleaseDir, [AppSpec|AppSpecs], Options) ->
    RootDir = epl_util:get_val(root_dir, Options),
    AppName = atom_to_list(element(1, AppSpec)),
    AppVsn  = element(2, AppSpec),
    AppDir = epl_installed_paths:app_dir(RootDir, AppName, AppVsn),
    case filelib:is_dir(AppDir) of
	false ->
	    IAppDir = epl_installed_paths:app_dir(ReleaseDir, AppName, AppVsn),
	    epl_installed_info:write(IAppDir, Options),
	    ?INFO(". ", []);
	true ->
	    ok
    end,
    write_info_to_all_apps(ReleaseDir, AppSpecs, Options);
write_info_to_all_apps(_ReleaseDir, [], _Options) ->
    ok.

back_up_bin_in_release(ReleaseDir, RelFileDir) ->
    BinDir = filename:join(ReleaseDir, "bin"),
    ewl_file:copy_dir(BinDir, filename:join(RelFileDir, "bin")).
    
stage_missing_apps(ReleaseDir, Options) ->
    MissingApps = missing_from_release(ReleaseDir, Options),
    FoundApps = fetch_missing_info(ReleaseDir, MissingApps, Options, []),
    ok = stage_found_apps(ReleaseDir, FoundApps).

stage_found_apps(ReleaseDir, [Info|T]) ->
    epl_install_driver:install_app(Info#package_info.path, ReleaseDir),
    stage_found_apps(ReleaseDir, T);
stage_found_apps(_ReleaseDir, []) ->
    ok.
    

fetch_missing_info(_ReleaseDir, [], _Options, []) ->
    [];
fetch_missing_info(ReleaseDir, [], _Options, Acc) ->
    throw(?UEX({missing_apps, {ReleaseDir, Acc}},
	       "Could not install the release because applications are~n" ++
	       "missing. Please install the following apps with~n" ++
	       "install-app ~p~n",
	       [Acc]));
fetch_missing_info(ReleaseDir, [{Name, Vsn}|T], Options, Acc) ->
    try
	[epl_installed_info:app(atom_to_list(Name), Vsn, Options)|
	 fetch_missing_info(ReleaseDir, T, Options, Acc)]
    catch
	_C:_E ->
	    fetch_missing_info(ReleaseDir, T, Options, [{Name, Vsn}|Acc])
    end.


missing_from_release(ReleaseDir, Options) ->
    case epl_validation:validate_apps_in_rel(ReleaseDir) of
	ok ->
	    [];
	{error, UnCheckedMissingApps} ->
	    missing_from_root(UnCheckedMissingApps, Options)
    end.

missing_from_root(UnCheckedMissingApps, Options) ->
    RootDir = epl_util:get_val(root_dir, Options),
    lists:foldl(fun({AppName, AppVsn}, Acc) ->
			InstalledPath =
			    epl_installed_paths:app_dir(RootDir,
							atom_to_list(AppName),
							AppVsn),
			case filelib:is_dir(InstalledPath) of
			    false ->
				[{AppName, AppVsn}|Acc];
			    true ->
				Msg = "application ~p is already installed.~n",
				?DEBUG(Msg, [InstalledPath]),
				Acc
			end
		end, [], UnCheckedMissingApps).

%%--------------------------------------------------------------------
%% @private
%% @doc Diff all the config files shared by two releases.
%% @spec diff_config(RootDir, RelName, RelVsn1, RelVsn2) -> Diffs
%% where
%%  Diffs = [{ConfigFilePath1, ConfigFilePath2, DiffTerms}]
%%   DiffTerms = [term()]
%% @end
%%--------------------------------------------------------------------
diff_config(RootDir, RelName, RelVsn1, RelVsn2) -> 
    try
	Rel1ConfigFilePaths = epl_root_dir_util:find_config_file_path(RootDir,
								      RelName,
								      RelVsn1),
	Rel2ConfigFilePaths = epl_root_dir_util:find_config_file_path(RootDir,
								      RelName,
								      RelVsn2),
	lists:foldl(
	  fun(Rel1Path, Acc) ->
		  case basename_member(Rel1Path, Rel2ConfigFilePaths) of
		      false ->
			  Acc;
		      Rel2Path ->
			  case ewl_config_diff:config_files(Rel1Path,
							    Rel2Path) of
			      []   -> Acc;
			      Diff -> [{Rel1Path, Rel2Path, Diff}|Acc]
			  end
		  end
	  end,
	  [],
	  Rel1ConfigFilePaths)
    catch
	_Type:Ex ->
	    ?DEBUG("error on config diff ~p~n", [Ex]),
	    []
    end.

basename_member(Target, [Path|T]) ->
    Basename = filename:basename(Path),
    case filename:basename(Target) of
	Basename -> Path;
	_        -> basename_member(Target, T)
    end;
basename_member(_Target, []) ->
    false.

