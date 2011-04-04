%%%-------------------------------------------------------------------
%%% @doc Functions used to validate that a package is well-formatted OTP. 
%%% @end
%%%-------------------------------------------------------------------
-module(epl_validation).

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([
	 validate_apps_in_rel/1,
	 validate_apps_in_rel/2,
	 validate_type/1,
	 verify_presence_of_erl_files/1,
	 erts_validation/1,
	 app_validation/1,
	 binary_app_validation/1,
	 unbuilt_app_validation/1,
	 release_validation/1
	]).

-type package_type() :: app_binary_specific | app_binary_generic | app_source | release | erts.

%%--------------------------------------------------------------------
%% Macros
%%--------------------------------------------------------------------
-define(BINARY_FILE_EXTENSIONS, ["cmx","py","bat","exe","so"]).

%% List of regexs that are compared against the output of the "file" command to determine if a
%% given file is a "binary" file or not
-define(BINARY_FILE_REGEX, [ "ELF .* executable",
                             "shared object",
                             "dynamically linked",
                             "ar archive"]).


%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------
-include("erlpl.hrl").
-include("eunit.hrl").

%%====================================================================
%% External functions
%%====================================================================

%% @doc Determine which if any apps specified by the rel file are 
%% missing from the lib dirs provided.
-spec validate_apps_in_rel(ReleaseDir::string(), LibDirs::list()) -> ok | {error, [{AppName::atom(), AppVsn::string()}]}.
validate_apps_in_rel(ReleaseDir, LibDirs) -> 
    RelFile  = epl_util:fetch_rel_file_from_release_package(ReleaseDir),
    AppSpecs = epl_otp_metadata_lib:consult_rel_file(app_specs, RelFile),
    case report_missing_apps(AppSpecs, LibDirs) of
	[] ->
	    ok;
	MissingAppSpecs ->
	    {error, [{element(1, E), element(2, E)} || E <- MissingAppSpecs]}
    end.

-spec validate_apps_in_rel(ReleaseDir::string()) -> ok | {error, [{AppName::string, AppVsn::string()}]}.
validate_apps_in_rel(ReleaseDir) -> 
    LibDir = epl_installed_paths:lib_dir(ReleaseDir),
    validate_apps_in_rel(ReleaseDir, [LibDir]).

    
%% @doc Determine the type of the package and make sure it is a valid instance of that type.
-spec validate_type(PackageDir::string()) -> package_type().
validate_type(PackageDir) ->
    val_edoc(PackageDir).

%% @doc boolean function to indicate if the package is of the type 
%%      in question.
%% @spec (PackageDir) -> true | {error, Reason}
edoc_validation(PackageDir) ->
    case filelib:is_file(filename:join([PackageDir, "edoc-info"])) of
	true  -> true;
	false -> {error, missing_edoc_info_file}
    end.

%% @doc boolean function to indicate if the package is of the type 
%%      in question.
%% @spec (PackageDir) -> true | {error, Reason}
erts_validation(PackageDir) ->
    try
    lists:all(fun(F) -> F(PackageDir) end, [
	
	%% Run all the following lambda's and if all of them return true then we have a well formed application.
	
	fun(PackageDir_) ->  
            case filelib:wildcard(PackageDir_ ++ "/include/driver_int.h") of
		[_|_] -> 
		    true;
		[] -> 
		    throw(does_not_contain_driver_int_h)
	    end
	end, 

	fun(PackageDir_) ->  
            case filelib:wildcard(PackageDir_ ++ "/include/erl_fixed_size_int_types.h") of
		[_|_] -> 
		    true;
		[] -> 
		    throw(does_not_contain_fixed_size_int_types_h)
	    end
	end 
    ])
    catch
	_C:E ->
	    {error, E}
    end.

%% @doc boolean function to indicate if the package is of the type 
%%      in question.
%% @spec (PackageDir) -> true | {error, Reason}
unbuilt_app_validation(AppDir) ->
    case length(filelib:wildcard(filename:join(AppDir, "ebin/*beam"))) of
	0 -> true;
	_ -> {error, contains_object_code}
    end.

%% @doc boolean function to indicate if the package is of the type 
%%      in question.
%% @spec (PackageDir) -> true | {error, Reason}
binary_app_validation(PackageDir) ->
    Resp = 
    lists:any(fun(F) -> F(PackageDir) end, [
	
	%% Run all the following lambda's and if any of them return true the package dir is a binary app and the function
	%% will return true.
	
	fun(PackageDir_) ->  
	    lists:any(fun(Dir) -> 
			      case re:run(Dir, ".*_src") of
				  {match, _} -> true;
				  _          -> false
			      end
		      end,
		      filelib:wildcard(PackageDir_ ++ "/*"))
	end, 
	
	fun(PackageDir_) ->
		RegexpBody = string:strip(lists:flatten([".*\\." ++ Ext ++ "$|" || Ext <- ?BINARY_FILE_EXTENSIONS]), right, $|), 
		Exts = lists:flatten(["(", RegexpBody, ")"]),
		case ewl_file:find(PackageDir_, Exts) of
		    []    -> false;
		    [_|_] -> true
		end
	end,

        fun(PackageDir_) ->
                Files = ewl_file:find(PackageDir_, ".*"),
                lists:any(fun is_binary_file/1, Files)
        end,

        fun(PackageDir_) ->
                has_binary_override_entry(PackageDir_)
        end
    ]),
    case Resp of
	true  -> true;
	false -> {error, contains_no_binary_files}
    end.
	    
		
%% @doc boolean function to indicate if the package is of the type 
%%      in question.
%% @spec (PackageDir) -> true | {error, Reason}
%% where
%%  Reason = missing_rel_file
release_validation(ReleaseDir) ->
    ReleasesDir = filename:join([ReleaseDir, "releases"]),
    case filelib:is_dir(ReleasesDir) of
	true ->
	    check_releases_dir_for_rel_file(ReleasesDir);
	false ->
	    check_release_for_rel_file(ReleaseDir)
    end.

check_release_for_rel_file(ReleaseDir) ->
    case filelib:wildcard(filename:join([ReleaseDir, "*.rel"])) of
	[] -> 
	    {error, missing_rel_file};
	%% XXX TODO verify release name cooresponds to dir name
	_RelFiles ->
	    true
    end.
    

check_releases_dir_for_rel_file(ReleasesDir) ->
    case filelib:wildcard(filename:join([ReleasesDir, "*/*.rel"])) of
	[] -> 
	    {error, missing_rel_file};
	RelFiles ->
	    duplicate_rel_dirs([filename:dirname(RelFile) || RelFile <- RelFiles])
    end.

duplicate_rel_dirs([RelDir|RelDirs]) ->
    case lists:member(RelDir, RelDirs) of
	true  -> {error, {multiple_rel_files_in_single_rel_dir, RelDir}};
	false -> duplicate_rel_dirs(RelDirs)
    end;
duplicate_rel_dirs([]) ->
    true.
	    
%% @doc boolean function to indicate if the package is of the type 
%%      in question.
%% @spec (PackageDir) -> true | {error, Reason}
%% where
%%  Reason = missing_app_file
app_validation(AppDir) ->
    case filelib:wildcard(filename:join([AppDir, "ebin", "*.app"])) of
	[] -> 
	    {error, missing_app_file};
	_ ->
	    true
    end.

%% @doc Make sure an application contains all the source files that the .app files suggests it does.
%% @spec verify_presence_of_erl_files(AppDir::string()) -> ok | {error, Reason}
verify_presence_of_erl_files(AppDir) ->
    F = fun(Mod) -> 
		case filelib:is_file(filename:join([AppDir, "src", atom_to_list(Mod) ++ ".erl"])) of
		    true -> 
			true;
		    false ->
			?DEBUG("~p is missing~n", [Mod]),
			false
		end
	end, 
    case catch lists:foreach(F, get_app_modules(AppDir)) of
	ok     -> ok;
	Error -> {error, {bad_app_missing_modules, AppDir, Error}}
    end.

%%====================================================================
%% Internal functions
%%====================================================================
get_app_modules(AppDir) ->
    epl_otp_metadata_lib:consult_app_file(
      modules,
      epl_otp_metadata_lib:app_file_path(AppDir)).


%% Predicate that uses the O/S supplied "file" command to determine if a given filename is an
%% executable
is_binary_file(Filename) ->
    FileType = os:cmd(io_lib:format("file -b ~s", [Filename])),
    lists:any(fun(Regex) ->
                      case re:run(FileType, Regex) of
                          {match, _} -> true;
                          _NoMatch   -> false
                      end
              end, ?BINARY_FILE_REGEX).
                              

%% Predicate that checks the application file for an override flag which will force the
%% app to be published as a "binary" app
has_binary_override_entry(PackageDir) ->
    case filelib:wildcard(PackageDir ++ "/ebin/*.app") of
        [File] ->
            case file:consult(File) of
                {ok, [{application, _, Keys}]} ->
                    proplists:get_bool(force_binary_app, Keys);
                _Other ->
                    false
            end;
        _ ->
            false
    end.
                        


val_edoc(PackageDir) ->
    case edoc_validation(PackageDir) of
	true -> {ok, edoc};
	_    -> val_erts(PackageDir)
    end.

val_erts(PackageDir) ->
    case erts_validation(PackageDir) of
	true -> {ok, erts};
	_    -> val_app(PackageDir)
    end.

val_app(PackageDir) ->
    case app_validation(PackageDir) of
	true ->
	    case unbuilt_app_validation(PackageDir) of
		true ->
		    {ok, app_source};
		_ ->
		    case binary_app_validation(PackageDir) of
			true  -> {ok, app_binary_specific};
			_     -> {ok, app_binary_generic}
		    end
	    end;
	_ ->
	    val_release(PackageDir)
    end.

val_release(PackageDir) ->
    case release_validation(PackageDir) of
	true  -> {ok, release};
	_     -> {error, {unrecognizable_package, PackageDir}}
    end.

report_missing_apps(AppSpecs, LibDirs) ->
    lists:filter(fun(AppSpec) -> not is_app_present(AppSpec, LibDirs) end, AppSpecs).
	    
is_app_present(AppSpec, LibDirs) ->    
    lists:any(fun(LibDir) ->
		      AppDir = epl_util:app_dir(AppSpec, LibDir),
		      filelib:is_dir(AppDir)
	      end,
	      LibDirs).
