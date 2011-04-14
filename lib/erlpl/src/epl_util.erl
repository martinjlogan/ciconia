%%% @author Martin Logan <martinjlogan@Macintosh.local>
%%% @copyright (C) 2010, Martin Logan
%%% @doc
%%% Misc functions not worth moving to a more general library.
%%% Created : 16 Jun 2010 by Martin Logan <martinjlogan@Macintosh.local>
-module(epl_util).

%% API
-export([
	 app_dir/2,
	 name_and_vsn/2,
	 fetch_rel_file_from_release_package/1,
	 unpack_to_tmp_if_archive/1,
	 copy_to_tmp/1,
	 assert_option/2,
	 assert_option/3,
	 get_option/4,
	 get_option/2,
	 get_val/3,
	 get_val/2,
         do_until/3, 
         do_until/2, 
	 force_or_prompt_for_an_action/5,
	 highest_vsn/1,
	 dedupe_tuple_list/2,
	 dictafy/1,
	 dictafy/2
	]).

-include("erlpl.hrl").

%%%===================================================================
%%% API
%%%===================================================================
%% @doc given an app spec and a lib dir return the cooresponding
%% app dir within a release package.
-spec app_dir(AppSpec::tuple(), LibDir::string()) -> string().
app_dir(AppSpec, LibDir) ->   
    {AppName, AppVsn} = extract_app_vsn_from_appspec(AppSpec),
    filename:join([LibDir, name_and_vsn(AppName, AppVsn)]).
	
extract_app_vsn_from_appspec(AppSpec) when is_tuple(AppSpec) ->
    {element(1, AppSpec), element(2, AppSpec)}.

%% @doc a function to contatinate an appname and its version
%% with a - in the middle. Yes, just that. 
-spec name_and_vsn(atom() | string(), string()) -> string().
name_and_vsn(AppName, AppVsn) when is_atom(AppName) -> name_and_vsn(atom_to_list(AppName), AppVsn);
name_and_vsn(AppName, AppVsn)                       -> AppName ++ "-" ++ AppVsn.

%% @doc take an compressed artifact and unpack it into a unique temporary directory and 
%%      return a path or paths to the resulting artifacts.
%% @spec unpack_to_tmp(ArtifactFilePath) -> Path | Paths
%% where
%%  Paths = [Path]
%%  Path = string()
unpack_to_tmp(ArtifactFilePath) ->
    TmpDirPath = ewl_file:make_tmp_dir(),
    ArtifactFileName = filename:basename(filename:absname(ArtifactFilePath)),
    TmpArtifactFilePath = ewl_file:join_paths(TmpDirPath, ArtifactFileName),
    ok = epl_file:copy(ArtifactFilePath, TmpArtifactFilePath),
    {ok, CWD} = file:get_cwd(),
    ok = file:set_cwd(TmpDirPath),
    ok = ewl_file:uncompress(ArtifactFileName),
    ok = epl_file:remove(ArtifactFileName),
    ok = file:set_cwd(CWD),
    case filelib:wildcard(TmpDirPath ++ "/*") of
        [] -> throw(?EX({failed_to_unpack, ArtifactFilePath}));
	[TmpArtifactPath] -> TmpArtifactPath;
	TmpArtifactPaths -> TmpArtifactPaths
    end.

%% @doc Applies a fun to all elements of a list until getting Return.
%% <pre>
%% This takes a fun, its expected return on 'success', and a list. The fun 
%% is applied to each element of the list untill the expected return is 
%% generated. If the expected return is generated from the application of 
%% the fun on an element of the list do_until halts and returns with Return. 
%% If not then the return value from the Fun being applied to the final 
%% element in the list is returned. If the list supplied is
%% empty then the return will be false. 
%% </pre>
%% @spec do_until(Fun, Return, list()) -> Return | Other | false
do_until(_F, _, []) ->
    false;
do_until(F, _, [Last]) ->
    F(Last);
do_until(F, Return, [H|T]) ->
    case F(H) of
	Return -> Return;
	_      -> do_until(F, Return, T)
    end.
    
%% @spec do_until(Fun, list()) -> bool()
%% @equiv do_until(Fun, true, list())
do_until(F, List) ->
    do_until(F, true, List).

%% @doc Remove the duplicates in a list of sorted tuples based on a
%% particular element in each tuple. 
%% @spec (Element, TupleList) -> NewList
dedupe_tuple_list(Element, [E1|T]) ->
    case dedupe_tuple_list(Element, T) of
	[]                                                           -> [E1];
	[E2|_] = L when element(Element, E1) == element(Element, E2) -> L;
	L          when is_list(L)                                   -> [E1|L];
	E2                                                           -> [E1, E2]
    end;
dedupe_tuple_list(_Element, []) ->
    [].


%% @doc find the highest version in a list of version strings.
%% @spec (Vsns::list()) -> Vsn::string()
highest_vsn(Vsns) when length(Vsns) > 0 ->
    hd(lists:sort(fun(A, B) -> ewr_util:is_version_greater(A, B) end, Vsns));
highest_vsn([]) ->
    [].


%% Assert that an option is present.
-spec assert_option(atom(), list()) -> ok.
assert_option(Option, Options) -> 
    case get_val(Option, Options) of
	undefined ->
	    Msg = "The option ~p is required and must be supplied.~n",
	    throw(?UEX({missing_option, Option, Options}, Msg, [Option]));
	_Value ->
	    ok
    end.

%% Assert that an option is present and well formed according to cmdln specs.
-spec assert_option(atom(), list(), tuple()) -> ok.
assert_option(Option, Options, Spec) -> 
    get_option(Option, Options, Spec, required),
    ok.
	    
%% @doc Fetch an options value. First look in the supplied options list
%%      and if not found check for an OS env value of the same name. 
-spec get_option(atom(), list(), get_opts_spec(), optional | required) -> term() | undefined.
get_option(Option, Options, {OptionSpecs, _, _}, Required) ->
    OptionNames = [element(1, OSpec) || OSpec <- OptionSpecs],
    case lists:member(Option, OptionNames) of
	true ->
	    return_val(get_option(Option, Options), Option, Required);
	false ->
	    Msg = "The option ~p supplied is not a valid option.~n" ++
		  "Please supply one of the following options:~n~p~n",
	    throw(?UEX({bad_option, Option}, Msg, [Option, OptionNames]))
    end.

-spec get_option(atom(), list()) -> term().
get_option(Option, Options) ->
    Value  = get_val(Option, Options),
    case Value of
	undefined ->
	    pull_env(Option);
	Value ->
	    Value
    end.

pull_env(Option) ->
    case os:getenv(atom_to_list(Option)) of
	false ->
	    undefined;
	Value ->
	    Value
    end.

return_val(undefined, Option, required) ->
    Msg = "The option ~p is required and must be supplied.~n",
    throw(?UEX({missing_option, Option}, Msg, [Option]));
return_val(Value, _Option, _Required) ->
    Value.



%% @doc fetch rel file from a release package.
-spec fetch_rel_file_from_release_package(PackageDir::string()) -> string().
fetch_rel_file_from_release_package(PackageDir) ->
    try
	[RelFile] = epl_otp_metadata_lib:rel_file_paths(PackageDir),
	RelFile
    catch
	_:_ -> 
	    throw(?UEX(multiple_rel_files, "The release package ~s~ncontaines more than one rel file.~n~p~n",
		       [PackageDir, (catch epl_otp_metadata_lib:rel_file_paths(PackageDir))]))
    end.
    
	    

%% @doc Fetch a value out of an options list. If the value is a unique
%%      value then return true when present and default when not.
%% @spec get_val(Key, TupleList, DefaultValue) -> Value | DefaultValue
get_val(Key, [Key|_], _DefaultValue)          -> true;
get_val(Key, [{Key, Value}|_], _DefaultValue) -> Value;
get_val(Key, [_|T],            DefaultValue)  -> get_val(Key, T, DefaultValue);
get_val(_Key, [],               DefaultValue) -> DefaultValue.

%% @spec get_val(Key, TupleList) -> Value | DefaultValue
%% @equiv get_val(Key, TupleList, DefaultValue)
get_val(Key, TwoTupleList) -> get_val(Key, TwoTupleList, undefined).


%% @doc Either do something (run the fun) or prompt first.
%% The user passes in a prompt, a set of valid responses, and a set of
%% reponses (subset of valid responses) that will triger the action.
%% @spec (Prompt, RespSet, SuccessSet, Fun, false) -> no_action | term()
force_or_prompt_for_an_action(_Prompt, _RespSet, _SuccessSet, Fun, true) ->
    Fun();
force_or_prompt_for_an_action(Prompt, RespSet, SuccessSet, Fun, false) ->
    Reply =  ask(Prompt, list, RespSet),
    case lists:member(Reply, SuccessSet) of
	true ->
	    Fun();
	_    ->
	    no_action
    end;
force_or_prompt_for_an_action(Prompt, RespSet, SuccessSet, Fun, Options) ->
    Force = epl_util:get_val(force, Options, false),
    force_or_prompt_for_an_action(Prompt, RespSet, SuccessSet, Fun, Force).

ask(Prompt, list, [H|_] = ResponseList) when is_list(H) ->
    Res = ewl_talk:ask(Prompt),
    case lists:member(Res, ResponseList) of
	true ->
	    Res;
	false ->
	    ewl_talk:say(lists:append(["Your answer must be "],
			     lists:reverse(["or \"" ++ H ++ "\""] ++ ["\"" ++ E ++ "\" " || E <- tl(ResponseList) ]))),
	    ask(Prompt, list, ResponseList)
    end.

%% @doc take a list of package info records and place them into a dict.
%%      keyed by name and sorted by version.
-spec dictafy(tuple() | list()) -> Dict::term().
dictafy(PackageList) ->
    dictafy(PackageList, dict:new()).

%% XXX TODO delete this if no other function uses anymore.
-spec dictafy(list() | tuple(), Dict::term()) -> term().
dictafy(PkgInfo, Dict) when is_tuple(PkgInfo) ->
    dictafy([PkgInfo], Dict);
dictafy([Row|T], Dict) ->
    Name = Row#package_info.name,
    NewDict =
	case dict:find(Name, Dict) of
	    {ok, NamePackageList} ->
		dict:store(Name, insert_row(Row, NamePackageList), Dict);
	    error ->
		dict:store(Name, [Row], Dict)
	end,
    dictafy(T, NewDict);
dictafy([], Dict) ->
    Dict.

%% @doc copy a directory or file to tmp and return the path
%% for the new location.
-spec copy_to_tmp(string()) -> string().
copy_to_tmp(ArtifactFilePath) ->
    TmpDirPath = ewl_file:make_tmp_dir(),
    ArtifactFileName    = filename:basename(filename:absname(ArtifactFilePath)),
    TmpArtifactFilePath = ewl_file:join_paths(TmpDirPath, ArtifactFileName),
    ewl_file:copy(ArtifactFilePath, TmpArtifactFilePath, [recursive]),
    TmpArtifactFilePath.

%% @doc if a package is a tarball then untar it into a tmp dir and
%%      hand back the path(s) to the unpacked contents of the temp dir.
%%      If the package is not an archive just move it to tmp
-spec unpack_to_tmp_if_archive(PackageDirPath::string()) -> DirPath::string().
unpack_to_tmp_if_archive(FilePath) ->
    case re:run(FilePath, ".*" ++ ?REPO_FILE_EXT_REGEXP ++ "$") of
	{match, _} ->
	    unpack_to_tmp(FilePath);
	_NoMatch ->
	    copy_to_tmp(FilePath)
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

insert_row(Row, Rows) ->
    Vsn = Row#package_info.vsn,
    insert_row(Vsn, Row, Rows).

insert_row(Vsn, Row, [Row2|_T] = Rows) ->
    Vsn2 = Row2#package_info.vsn,
    case ec_string:compare_versions(Vsn, Vsn2) of
	true ->
	    [Row|Rows];
	false ->
	    dedup(Vsn, Vsn2, Row, Rows)
    end;
insert_row(_Vsn, Row, []) ->
    [Row].

dedup(Vsn, Vsn, Row, [Row2|T] = Rows) ->
    IsEqual = 
	{Row#package_info.repo, Row#package_info.path} == 
	{Row2#package_info.repo, Row2#package_info.path},
    case IsEqual of
	true -> Rows;
	false -> [Row2|insert_row(Vsn, Row, T)]
    end;
dedup(Vsn, _Vsn2, Row, [Row2|T]) ->
    [Row2|insert_row(Vsn, Row, T)].

%%%===================================================================
%%% Testing Functions
%%%===================================================================

-ifndef(NOTEST).
-include_lib("eunit/include/eunit.hrl").

dictafy_test() ->
    Expected = [
		#package_info{name = faxien, vsn = "2.2.3"},
		#package_info{name = faxien, vsn = "2.2.3-rc2"},
		#package_info{name = faxien, vsn = "2.2.3-rc1"},
		#package_info{name = faxien, vsn = "1.2.3"},
		#package_info{name = faxien, vsn = "1.2.3-rc1"}
	       ],

    ?assertMatch(Expected, dict:fetch(faxien, dictafy(lists:reverse(Expected)))).
    
-endif.
