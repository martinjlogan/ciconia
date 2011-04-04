%%%-------------------------------------------------------------------
%%% @author Martin Logan <martinjlogan@Macintosh.local>
%%% @copyright (C) 2010, Martin Logan
%%% @doc
%%%  Contains functions for querying and manipulating otp metadata.
%%% @end
%%% Created : 13 Jun 2010 by Martin Logan <martinjlogan@Macintosh.local>
%%%-------------------------------------------------------------------
-module(epl_otp_metadata_lib).

%% API
-export([
	 package_info/2,
	 app_name_and_vsn/1,
	 package_dir_to_name_and_vsn/1,
	 app_compiler_vsns/1,
	 app_file_path/1,
	 rel_file_paths/1,
	 consult_app_file/2,
	 consult_rel_file/2
	]).

-include("erlpl.hrl").
-include_lib("kernel/include/file.hrl").

%%%===================================================================
%%% API
%%%===================================================================

%% @doc given a an OTP app directory return the full path to the
%%      dot app file.
-spec app_file_path(string()) -> string().
app_file_path(AppDir) ->
    case filelib:wildcard(filename:join([AppDir, "ebin", "*.app"])) of
	[] ->
	    Msg = "The .app file is missing from the application at ~p~nAll OTP application packages must " ++
		  "have a .app file~n",
	    throw(?UEX({app_files_missing, AppDir}, Msg, [AppDir]));
	[AppFilePath] ->
	    AppFilePath
    end.


%% @doc given a an OTP release directory return the full path to all 
%%      the dot rel files found.
-spec rel_file_paths(list()) -> [list()].
rel_file_paths(RelDir) ->
    case ewl_file:find(RelDir, ".*.rel$") of 
	[] ->
           Msg = "The rel file is missing from the release at ~p~nAll OTP release packages must have at least one .rel file~n",
           throw(?UEX({rel_files_missing, RelDir}, Msg, [RelDir]));
	RelFilePaths ->
           RelFilePaths
    end.


%% @doc Returns an element or list of elements from a .app file. Note
%%      that using the key 'name' will return the app name.
%% @spec (Keys, AppFilePath) -> [Value] | Value | {error, Reason}
%% where
%%  Keys = Key | [Key]
%%  Reason = badly_formatted_app_file | enoent 
-spec consult_app_file([atom()], list()) -> [term()] | term().
consult_app_file(Key, AppFilePath) when is_atom(Key) ->
    case consult_app_file([Key], AppFilePath) of
	[Value] -> Value;
	Error   -> throw(?EX(Error))
    end;
consult_app_file(Keys, AppFilePath) ->
    case file:consult(AppFilePath) of
	{ok, [{application, AppName, AppList}]} ->
	    consult_key_values(Keys, [{name, AppName}|AppList]); 
	{error, _} ->
	    Msg = "The .app file at ~p~ndoes not appear to exist or you lack the permissions to read it. Please check~n" ++
		  "to see if the file exists and that it has the correct permissions~n",
	    throw(?UEX({failed_to_read_app_file, AppFilePath}, Msg, [AppFilePath]));
	{ok, _BadTerm} ->
	    Msg = "The .app file at ~p~nis malformed. Please review the file and correct it.~n",
	    throw(?UEX({badly_formatted_app_file, AppFilePath}, Msg, [AppFilePath]))
    end.


%% @doc Returns a list of the elements that correspond to the keys that were supplied.
%% @spec consult_rel_file(Keys, RelFilePath) -> [Value] | Value | {error, Reason}
%% where
%%  Keys = Key | [Key]
%%   Key = name | vsn | erts_vsn | app_specs
%%  Reason = badly_formatted_rel_file | enoent 
consult_rel_file(Key, RelFilePath) when is_atom(Key) ->
    case consult_rel_file([Key], RelFilePath) of
	[Value] -> Value;
	Error   -> throw(?EX(Error))
    end;
consult_rel_file(Keys, RelFilePath) ->
    case file:consult(RelFilePath) of
	{ok, [RelTerm]} ->
	    lists:map(fun(Key) -> extract_rel_value(Key, RelTerm) end, Keys);
	{error, _} ->
	    Msg = "The release file at ~p~ndoes not appear to exist or you lack the permissions to read it. Please check~n" ++
		  "to see if the file exists and that it has the correct permissions~n",
	    throw(?UEX({failed_to_read_rel_file, RelFilePath}, Msg, [RelFilePath]));
	{ok, _BadTerm} ->
	    Msg = "The release file at ~p~nis malformed. Please review the file and correct it.~n",
	    throw(?UEX({badly_formatted_rel_file, RelFilePath}, Msg, [RelFilePath]))
    end.


%% @private
%% @doc Fetch the compiler version that all modules in the application were compiled with.
%% @spec get_compiler_vsn(AppDir) -> CompilerVersions::list()
app_compiler_vsns(AppDir) ->
    Modules = consult_app_file(modules, app_file_path(AppDir)),
    case Modules of
	[] ->
	    Msg = "The application ~p~ndoes not contain any Erlang modules.~n" ++
		"All apps must contain Erlang modules~n",
	    throw(?UEX({empty_module_list_for_app, AppDir}, Msg, [AppDir]));
	Modules ->
	    sort_vsn_list(get_compiler_vsns(AppDir, Modules))
    end.

%% @doc Fetch the application name and version from a valid otp app.
%% @spec (AppDir::string()) -> {AppName::string(), AppVsn::string()} 
app_name_and_vsn(AppDir) ->
    [AppName, AppVsn] =
	consult_app_file([name, vsn], app_file_path(AppDir)),
    {atom_to_list(AppName), AppVsn}.

%% @doc
%% returns the name and version of a package for a package directory:
%% Example:
%%  package_dir_to_name_and_vsn("/usr/local/erlware/releases/my_rel/1.2.4") -> {"my_rel". "1.2.4"}
%%  package_dir_to_name_and_vsn("/home/martin/my_rel-1.2.4") -> {"my_rel". "1.2.4"}
%%
%% @spec package_dir_to_name_and_vsn(RawPackageDir::string()) -> {Name, Version} 
package_dir_to_name_and_vsn(RawPackageDir) ->
    PackageDir = filename:basename(filename:absname(RawPackageDir)),
    case re:run(PackageDir, ?PACKAGE_NAME_AND_VSN_REGEXP) of
	{match, [{0, _}|_]} ->
	    {ok, {[PackageName], PackageVsn}} = ewl_string_manip:n_tokens(PackageDir, 1, "-"),
	    {PackageName, lop_off_end(PackageVsn)};
	_Error -> 
	    Msg = "The package name ~s is not of the form <name>-<version>~nAll package " ++
		  "names must have this form.~n",
	    throw(?UEX({bad_package_name, RawPackageDir}, Msg, [RawPackageDir]))
    end.

lop_off_end(Vsn) ->
    lop_off_end2(lists:reverse(Vsn)).

%% Lop of suffixes like .epkg and .tar.gz
lop_off_end2([$g,$k,$p,$e,$.|Vsn]) ->
    lists:reverse(Vsn);
lop_off_end2([$z,$g,$.,$r,$a,$t,$.|Vsn]) ->
    lists:reverse(Vsn);
lop_off_end2([$r,$a,$j,$.|Vsn]) ->
    lists:reverse(Vsn);
lop_off_end2(Vsn) ->
    lists:reverse(Vsn).

%% @doc Fetch relivant information about a local package. 
-spec package_info(string(), list()) -> tuple().
package_info(PackageDir, Options) ->
    ?DEBUG("fetching package info for ~p~n", [PackageDir]),
    RootDir = epl_util:get_option(root_dir, Options),
    {Name, Vsn} = epl_otp_metadata_lib:package_dir_to_name_and_vsn(PackageDir),
    {ok, #file_info{mtime = MTime}} = file:read_file_info(PackageDir),
    PackageBin = package_bin(PackageDir),
    Checksum = ewl_file:md5_checksum(PackageBin),
    TimeStamp = calendar:datetime_to_gregorian_seconds(MTime),
    {ok, Type} = epl_validation:validate_type(PackageDir),
    #package_info{name         = Name,
		  vsn          = Vsn,
		  checksum     = Checksum,
		  timestamp    = TimeStamp,
		  root_dir     = RootDir,
		  path         = filename:absname(package_dir(Type, Name, Vsn, RootDir)),
		  package_type = Type,
		  meta         = meta_data(Type, PackageDir) 
		 }.

meta_data(release, PackageDir) ->
    [{config_terms, config_file_checksums(PackageDir)},
     {bin_files, bin_files(PackageDir)}];
meta_data(_Type, _PackageDir) ->
    [].

package_dir(release, Name, Vsn, RootDir) ->
    epl_installed_paths:release_dir(RootDir, Name, Vsn);
package_dir(erts, "erts", Vsn, RootDir) ->
    epl_installed_paths:erts_dir(RootDir, Vsn);
package_dir(_AppTypes, Name, Vsn, RootDir) ->
    % Perhaps we could validate here against app types
    epl_installed_paths:app_dir(RootDir, Name, Vsn).
    
    
%%%===================================================================
%%% Internal functions
%%%===================================================================
get_compiler_vsns(AppDir, Modules) ->
    get_compiler_vsns(AppDir, Modules, []).

get_compiler_vsns(AppDir, [Module|Modules], CompilerVsns) ->
    case fetch_vsn(AppDir, Module) of
        missing_module ->
	    % Module was missing, no compiler info available, but since the file doesn't
	    % exist, the compiler info is irrelevant; log a warning but continue on
            ?WARN("the beam file for ~p which is listed in the .app at ~p doesn't actually exist!", [Module, AppDir]),
            get_compiler_vsns(AppDir, Modules, CompilerVsns);
	CompilerVsn ->
	    case lists:member(CompilerVsn, CompilerVsns) of
		true ->
		    get_compiler_vsns(AppDir, Modules, CompilerVsns);
		false ->
		    get_compiler_vsns(AppDir, Modules, [CompilerVsn|CompilerVsns])
	    end
    end;
get_compiler_vsns(_AppDir, [], CompilerVsns) ->
    CompilerVsns.
	
fetch_vsn(AppDir, Module) ->
    BeamPath  = AppDir ++ "/ebin/" ++ atom_to_list(Module),
    case catch beam_lib:chunks(BeamPath, [compile_info]) of
        {ok, {Module, [{compile_info, CompileInfo}]}} ->
            case epl_util:get_val(version, CompileInfo) of
                undefined ->
		    Msg = "The beam at ~p~ndoes not have a compiler version in its metadata. Please recompile with~n" ++
			  "the correct flags. Check the docs at erlang.org for erlc to learn more~n",
                    throw(?UEX({no_compiler_vsn_found, BeamPath}, Msg, [BeamPath]));
                Vsn ->
                    Vsn
            end;
        {error, beam_lib, {file_error, _, enoent}} ->
            %% Arguably, if a .beam is listed in a .app, it shouldn't cause the 
            %% entire publish to fail. We know of at least one case in the core Erlang
            %% distribution (hipe) where modules are listed that actually live within
            %% the VM. Therefore, notify the caller that the module doesn't exist
            %% but don't make everything blow up.
            missing_module;
        Error ->
            throw(?EX(Error))
    end.

sort_vsn_list(VsnList) ->
    lists:sort(fun(V1, V2) -> ec_string:compare_versions(V1, V2) end, VsnList).

%% Extract a value from a .rel term.
extract_rel_value(name, {release, {Name, _}, _, _}) ->
    Name;
extract_rel_value(vsn, {release, {_, Vsn}, _, _}) ->
    Vsn;
extract_rel_value(erts_vsn, {release, _, {erts, ErtsVsn}, _}) ->
    ErtsVsn;
extract_rel_value(app_specs, {release, _, _, AppSpecs}) ->
    AppSpecs;
extract_rel_value(_, Junk) ->
    Msg = "The release file is malformed. Please review the file and correct it.~nContents are currently:~n~p~n",
    throw(?UEX({badly_formatted_rel_file, Junk}, Msg, [Junk])).

consult_key_values(Keys, KeyValues) ->
    lists:foldr(fun(Key, Acc) -> 
			case epl_util:get_val(Key, KeyValues) of
			    undefined -> Acc;
			    Value     -> [Value|Acc]
			end
		end, [], Keys).
    

bin_files(PackageDir) ->
    BinDir = epl_installed_paths:bin_dir(PackageDir),
    BinFiles = filelib:wildcard(filename:join(BinDir, "*")),
    lists:foldl(fun(FileName, Acc) ->
			case filelib:is_file(FileName) of
			    true  ->
				[{filename:basename(FileName),
				  epl_file:read(FileName)}|Acc];
			    false ->
				Acc
			end
		end,
		[],
		BinFiles).
    
config_file_checksums(RawRelDir) ->
    ReleasesDir = RawRelDir ++ "/releases",
    RelDir = 
	case filelib:is_dir(ReleasesDir) of
	    true -> ReleasesDir;
	    false -> RawRelDir
	end,
	
    case ewl_file:find(RelDir, ".*config$") of
        [] ->
            undefined;
        ConfigFiles ->
            ?DEBUG("Found the following config files ~p in ~p~n", [ConfigFiles, RelDir]),
	    lists:foldl(fun(ConfigFile, Acc) ->
                              try
				[{filename:basename(ConfigFile),
				  ewl_file:md5_checksum(epl_file:read(ConfigFile))}|Acc]
                              catch
                                  _C:_E -> Acc
                              end
                          end, [], ConfigFiles)
   end.

package_bin(PackageDir) ->
    TarFilePath = filename:join([create_tmp_dir(), filename:basename(PackageDir) ++ ".tar.gz"]),
    compress_package(TarFilePath, PackageDir),
    Bin = epl_file:read(TarFilePath),
    ewl_file:delete_dir(filename:dirname(TarFilePath)),
    Bin.

compress_package(TarFilePath, PackageDir) ->
    try
	ok = ewl_file:compress(TarFilePath, [filename:basename(PackageDir)],
			       [compressed, {cd, filename:dirname(PackageDir)}])
    catch
	_C:E ->
	    Msg = "Failed to compress and store ~p.~nEnsure your filesystem is not full~n",
	    throw(?UEX({failed_to_compress_package, E}, Msg, [PackageDir]))
    end.
	    

create_tmp_dir() ->
    try
	ewl_file:make_tmp_dir()
    catch
	_C:E ->
	    Msg = "Failed to create a temporary directory.~nEnsure your filesystem is not full~n",
	    throw(?UEX({failed_to_create_directory, E}, Msg, []))
    end.
