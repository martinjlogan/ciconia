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
-module(ep_cmd_publish).

%% API
-export([run/2, spec/0, description/0]).

-include("erlpl.hrl").
-include("eunit.hrl").

%%%===================================================================
%%% API
%%%===================================================================

%% @doc List installed packages.
-spec run(RawPackageDir::string(), Options::list()) -> ok.
run(RawPackageDir, Options) ->
    epl_util:assert_option(repo_type, Options, spec()),
    epl_util:assert_option(repos, Options, spec()),
    epl_util:assert_option(timeout, Options),
    PackageDir = epl_util:unpack_to_tmp_if_archive(RawPackageDir),
    publish_directory(PackageDir, Options).

description() ->
    "publish packages".

-spec spec() -> get_opts_spec().
spec() ->
    CmdLnTail = "",
    OptionsTail = "",
    OptionSpecs =
	[
      %% {Name,   ShortOpt, LongOpt,        ArgSpec,           HelpMsg}
	 {repo_type, $a,      "repo_type",  atom,  
	  "specifiy the repo type here. " ++
	  "\"faxien\" | \"dav\" | \"couchdb\" | \"agner\""},
	 {verbose, $v,      "verbose",      undefined,
	  "Option for verbose output"},
	 {erts_vsn, $e,      "erts_vsn",    string,
	  "Manually specify the erts version for an application" ++
	  " package only."},
	 {repos,   $r,      "repos",
	  {string, "http://repo.erlware.org/writable"},
	  "The repos to search"},
	 {recursive, $R,    "recursive",    undefined,
	  "recursively search the directory tree and publish all packages"},
	 {timeout, $t,      "timeout",      {integer, 60000},
	  "The timeout value for the operation"}
	],
    {OptionSpecs, CmdLnTail, OptionsTail}.

%%====================================================================
%% Internal functions
%%====================================================================

%% @doc return the type for a given OTP package.
%% See epl_validation for more details on possible types.
-spec package_type(PackageDir::string()) -> atom().
package_type(PackageDir) ->
    case epl_validation:validate_type(PackageDir) of
	{ok, Type} ->
	    Type;
	{error, Reason} ->
	    throw(?UEX(Reason,
		       "Validation of your package failed with ~p~n" ++
		       "Please inspect the package and fix it",
		       [Reason]))
    end.
    
publish_directory(PackageDir, Options) ->
    case filelib:is_dir(PackageDir) of
	true ->
	    Repos = epl_util:get_option(repos, Options, spec(), required),
	    validate_and_publish(Repos, PackageDir, Options);
	false ->
	    {error, not_directory}
    end.

validate_and_publish(Repos, PackageDir, Options) ->
    Recursive = epl_util:get_option(recursive, Options, spec(), optional),
    try package_type(PackageDir) of
	release = Type ->
	    publish_individual(Type, Repos, PackageDir, Options),
	    publish_sub_dir(Type, Recursive, PackageDir, Options);
	Type ->
	    publish_individual(Type, Repos, PackageDir, Options)
    catch
	_C:_E ->
	    publish_sub_dir(undefined, Recursive, PackageDir, Options)
    end.

publish_sub_dir(undefined, Recursive, _PackageDir, _Options)
  when Recursive /= true ->
    throw(?UEX(unrecognized_package_type,
	  "Ensure the package you are trying to publish is a valid OTP~n" ++
	  "package. If you are trying to publish a directory~n" ++
	  "containing valid packages try the -R recursive publish~n" ++
	  "option.",
	  []));
publish_sub_dir(_Type, true, PackageDir, Options) ->
    lists:foreach(fun(File) -> publish_directory(File, Options) end,
		  filelib:wildcard(ewl_file:join_paths(PackageDir, "*")));
publish_sub_dir(_Type, _False, _PackageDir, _Options) ->
    ok.
    
publish_individual(Type, Repos, PackageDir, Options) ->
    PackageBaseName = filename:basename(PackageDir),
    ?INFO("Publishing ~s of type ~p~n", [PackageBaseName, Type]), 
    case publish(Type, Repos, PackageDir, Options) of
	[] -> 
	    ?INFO(" > Duplicate package ~p not published to any repo~n",
		  [PackageBaseName]); 
	[Location] ->
	    ?INFO(" > Published ~p to ~p~n", [PackageBaseName, Location]); 
	Locations ->
	    ?INFO(" > Published ~p to ~p~n", [PackageBaseName, Locations]) 
    end.

publish(Type, Repos, PackageDir, Options)
  when Type == app_binary_specific; Type == app_binary_generic ->
    ErtsVsn = erts_vsn(PackageDir, Options),
    PackageType = type_to_repo_package_type_string(Type),
    {AppName, AppVsn} = epl_otp_metadata_lib:app_name_and_vsn(PackageDir),
    AppFile = read_app_file(PackageDir),
    PackageContext = 
	case Type of
	    app_binary_generic ->
		package_context(undefined, undefined, undefined, undefined,
				"erts-" ++ ErtsVsn, PackageType, AppName,
				AppVsn, [AppFile]);
	    app_binary_specific ->
		package_context(epl_os_specific:os_name(),
				epl_os_specific:os_release(),
				epl_os_specific:hardware_name(),
				epl_os_specific:glibc_version(),
				"erts-" ++ ErtsVsn, PackageType,
				AppName, AppVsn, [AppFile])
	end,
    put_package(Repos, PackageDir, PackageContext, Options);
publish(app_source = Type, Repos, PackageDir, Options) ->
    ErtsVsn = epl_util:get_val(erts_vsn, Options),
    PackageType = type_to_repo_package_type_string(Type),
    {AppName, AppVsn} = epl_otp_metadata_lib:app_name_and_vsn(PackageDir),
    AppFile = read_app_file(PackageDir),
    PackageContext = package_context(undefined, undefined, undefined,
				     undefined, "erts-" ++ ErtsVsn,
				     PackageType, AppName, AppVsn, [AppFile]),
    put_package(Repos, PackageDir, PackageContext, Options);
publish(release = Type, Repos, OrigPackageDir, Options) ->
    FilesPatternsToBeIgnored = ["erts-*", ".git", "lib", "install.sh"],
    PackageDir =
	remove_unpublishable_files_by_pattern(OrigPackageDir,
					      FilesPatternsToBeIgnored),
    PackageType = type_to_repo_package_type_string(Type),
    {Name, Vsn} = epl_otp_metadata_lib:package_dir_to_name_and_vsn(PackageDir),
    RelFilePath = epl_util:fetch_rel_file_from_release_package(PackageDir),
    ErtsVsn = epl_otp_metadata_lib:consult_rel_file(erts_vsn, RelFilePath),
    RelFile = read_rel_file(PackageDir),
    PackageContext = package_context(undefined, undefined, undefined,
				     undefined, "erts-" ++ ErtsVsn,
				     PackageType, Name, Vsn, [RelFile]),
    put_package(Repos, PackageDir, PackageContext, Options);
publish(erts = Type, Repos, PackageDir, Options) ->
    PackageType = type_to_repo_package_type_string(Type),
    {Name = "erts", Vsn} =
	epl_otp_metadata_lib:package_dir_to_name_and_vsn(PackageDir),
    PackageContext = package_context(epl_os_specific:os_name(),
				     epl_os_specific:os_release(),
				     epl_os_specific:hardware_name(),
				     epl_os_specific:glibc_version(),
				     undefined, PackageType, Name, Vsn, []),
    put_package(Repos, PackageDir, PackageContext, Options).

put_package(Repos, PackageDir, PackageContext, Options) ->
    Timeout = epl_util:get_option(timeout, Options),
    Driver = ep_util:driver(Options),
    Driver:put(Repos, PackageDir, PackageContext, Timeout).
    
erts_vsn(AppDir, Options) ->
	case epl_util:get_val(erts_vsn, Options) of
	    undefined ->
		erts_vsn_from_beam(AppDir, Options);
	    ErtsVsn_ ->
		ErtsVsn_
	end.

erts_vsn_from_beam(AppDir, Options) ->
    case discover_app_erts_vsns(AppDir, Options) of
	[ErtsVsn|_] ->
	    ErtsVsn;
	Error ->
	    ?DEBUG("beams compiled with an unsuppored erts vsn. Error ~p~n",
		   [Error]),
	    throw(?UEX(unsuported_erts_vsn,
		       "the erts version you are trying to publish~n" ++
		       "is not supported",
		       []))
    end.


%% @doc Discover what erts versions all beams in an app were compiled with.
%% @spec (AppDir, Options) -> {ok, [ErtsVsn]} | {error, Reason}
discover_app_erts_vsns(AppDir, Options) ->
    case length(filelib:wildcard(AppDir ++ "/ebin/*beam")) of
	0 ->
	    {error, no_beam_files};
	_NumBeams ->
	    CompilerVsns = epl_otp_metadata_lib:app_compiler_vsns(AppDir),
	    #package_info{executable_env = [$e, $r, $t, $s, $-|ErtsVsn]}
		= get_erts_vsn_for_compiler_vsn(hd(CompilerVsns), Options),
	    [ErtsVsn]
    end.

get_erts_vsn_for_compiler_vsn(CompilerVsn, Options) ->
    try 
	Packages = ep_cache:fetch("compiler", Options),
	ec_lists:fetch(fun(#package_info{vsn = Vsn}) ->
			       CompilerVsn == Vsn
		       end, Packages)
    catch
	_C:E ->
	    throw(?UEX({could_not_determine_erts_vsn, E},
		 "The erts vsn for this package could not be determined~n" ++
		 "Try updateing your package cache with the update-cache~n" ++
		 "command. You may also manually specify the erts version~n" ++
		 "with the -e flag (be sure to get it right)~n" ++
		 "*NOTE* the package was compiled with compiler-~s~n",
		 [CompilerVsn]))
    end.

read_app_file(AppPackagePath) ->
    epl_file:consult(epl_otp_metadata_lib:app_file_path(AppPackagePath)).

read_rel_file(TSPackagePath) ->
    epl_file:consult(
      epl_util:fetch_rel_file_from_release_package(TSPackagePath)).

package_context(OSName, OSRelease, HardwareName, GlibC, ExecutableEnv,
		PackageType, PackageName, PackageVsn, PackageMeta) ->
    #package_info{os_name  = OSName,
		  os_release = OSRelease,
		  hardware_name = HardwareName,
		  glibc_vsn = GlibC,
		  executable_env = ExecutableEnv,
		  package_type   = PackageType,
		  name   = PackageName,
		  vsn   = PackageVsn,
		  meta   = PackageMeta}.


type_to_repo_package_type_string(erts) ->
    "erts";
type_to_repo_package_type_string(release) ->
    "release";
type_to_repo_package_type_string(app_source) ->
    "app--source";
type_to_repo_package_type_string(app_binary_generic) ->
    "app--binary";
type_to_repo_package_type_string(app_binary_specific) ->
    "app--binary".

%% Non destructively remove files that match the patterns passed in.
%% Copy the release dir to a temp dir and then remove them
remove_unpublishable_files_by_pattern(RelDir, FilePatternsToBeIgnored) ->
    FilesToBeIgnored = files_to_be_ignored(RelDir, FilePatternsToBeIgnored),
    case do_ignore_files_exist(FilesToBeIgnored) of
	true ->
	    TmpRelDir = copy_to_tmp_dir(RelDir),
	    remove_files_from_dir(FilesToBeIgnored, TmpRelDir),
	    TmpRelDir;
	false ->
	    RelDir
    end.

files_to_be_ignored(Dir, FilePatternsToBeIgnored) ->
    Files = 
	lists:foldl(fun(Pattern, Acc) ->
			    Acc ++ filelib:wildcard(
				     ewl_file:join_paths(Dir, Pattern))
		    end, [], FilePatternsToBeIgnored),
    [filename:basename(F) || F <- Files].

do_ignore_files_exist(FilesToBeIgnored) ->
    lists:any(fun(File) ->
		      filelib:is_file(File)
	      end,
	      FilesToBeIgnored).

copy_to_tmp_dir(FilePath) ->
    TmpDir = ewl_file:make_tmp_dir(),
    FileName = filename:basename(filename:absname(FilePath)),
    TmpFilePath = ewl_file:join_paths(TmpDir, FileName),
    epl_file:copy(FilePath, TmpFilePath, [recursive]),
    TmpFilePath.
    

remove_files_from_dir(Files, Dir) ->
    lists:foreach(fun(File) ->
			  ?DEBUG("Removing ~p from release at ~p~n",
				 [File, Dir]),
			  epl_file:remove(ewl_file:join_paths(Dir, File),
					  [recursive])
		  end, Files).

