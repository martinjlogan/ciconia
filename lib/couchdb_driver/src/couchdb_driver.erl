%%%-------------------------------------------------------------------
%%% @doc
%%%  Functions specific to publishing to a couchdb repo type.
%%% @end
%%%-------------------------------------------------------------------
-module(couchdb_driver).

%% API
-export([put/4, list/3, get/2]).

-include("erlpl.hrl").
-include_lib("couchbeam/include/couchbeam.hrl").

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%%--------------------------------------------------------------------

%% @doc this function is used to list out all data in a repo based
%%      on a search criteria provided as a package context record.
%% This returns a list of package info records.
-spec list(list(), [record()], non_neg_integer()) -> list().
list(Repos, PackageInfo, _Timeout) when is_list(Repos) ->
    % XXX TODO changed to lists map for exception propagation.
    % This can change when pmap handles exceptions correctly.
    % this temp hack destroys timeouts.
    lists:map(fun(RepoURL) ->
                      list(RepoURL, PackageInfo)
              end, Repos).

-spec list(record(), [record()]) -> [[{binary(), term()}]].
list(RepoURL, PackageInfo) ->
    #package_info{executable_env = ExecutableEnv,
		  package_type   = PackageType,
		  name   = _PackageName,
		  meta   = PackageMeta} = PackageInfo,
    SysStringTuple = parse_sys_string(system_string(PackageInfo)),
    {StartKey, EndKey} = construct_start_and_end_keys(SysStringTuple, ExecutableEnv, PackageType, PackageMeta),
    {ok, CouchRepo} = get_couch_repo(RepoURL),
    {ok, View} = couchbeam:view(CouchRepo, "all/all", [{startkey, StartKey}, {endkey, EndKey}, {inclusive_end, true}]),
    parse_list_results(RepoURL, couchbeam_view_fetch(RepoURL, View)).

couchbeam_view_fetch(RepoURL, View) ->
    try
        {ok, {Rst2}} = couchbeam_view:fetch(View),
        Rst2
    catch
        _C:E ->
            throw(?UEX({repo_connect_failure, {E, RepoURL}},
                       "The repo specified by ~p~ncould not be contacted." ++
                       " Check your net connection~nor try your search without the" ++
                       " offending repo.",
                       [RepoURL]))
    end.


%% @doc this function is used to get a package from the repo
%%      *NOTE* I don't yet know what package spec is?
-spec get(record(), non_neg_integer()) -> term().
get(PackageInfo, Timeout) ->
    PackageMeta = PackageInfo#package_info.meta,
    RepoURL = PackageInfo#package_info.repo,
    PackageRepoID = epl_util:get_val(<<"_id">>, PackageMeta),
    {[{PackageName, _}]} = epl_util:get_val(<<"_attachments">>, PackageMeta),
    {ok, CouchRepo} = get_couch_repo(RepoURL),
    try
        couchbeam:fetch_attachment(CouchRepo, PackageRepoID, PackageName, [], Timeout)
    catch
        _C:E ->
            throw(?UEX({repo_connect_failure, {E, RepoURL}},
                       "The repo specified by ~p~ncould not be contacted." ++
                       " Check your net connection~nor try your search without the" ++
                       " offending repo.",
                       [RepoURL]))
    end.

%%--------------------------------------------------------------------
%% @doc
%% Notes for Tristan:
%  "erts" | "release" | "app--source" | "app--binary" | "app--binary".
%% @end
%%--------------------------------------------------------------------
-spec put(list(), string(), record(), non_neg_integer()) -> list().
put(Repos, PackageDir, PackageInfo, _Timeout) ->
    % os_family can be "any" or an OSFamily string. See the ep_publish module for how it is created. right now the only
    % allowed values are "linux" | "macintosh"
    PackageBin = ep_util:pack(PackageDir),
    Checksum = list_to_binary(io_lib:fwrite("~s", [ewl_file:md5_checksum(PackageBin)])),

    % XXX TODO removeing plist filter for regular filter until we can get exceptions working
    ?DEBUG("publish to ~p~n", [Repos]),
    lists:filter(fun(Repo) ->
                         case add_package(Repo, PackageBin, Checksum, PackageInfo) of
                             already_exists ->
                                 false;
                             _ ->
                                 true
                         end
                 end, Repos).

%%%===================================================================
%%% Internal functions
%%%===================================================================

parse_sys_string(SystemString) ->
    case re:run(SystemString, ".*\\[(\\d+)-(\\d+)\\].*", [{capture,all_but_first,binary}]) of
        {match,[High,Low]} ->
            HighString = re:replace(SystemString, "\\[(\\d+)-(\\d+)\\]", High, [{return, binary}]),
            LowString = re:replace(SystemString, "\\[(\\d+)-(\\d+)\\]", Low, [{return, binary}]),
            {LowString, HighString};
        nomatch ->
            SystemString
    end.

parse_list_results(RepoURL, [{<<"total_rows">>, _TotalRows},
                             {<<"offset">>, _Offset},
                             {<<"rows">>, Rows}]) ->
    lists:map(fun({[{<<"id">>, _ID}, {<<"key">>, _Key}, {<<"value">>, {Value}}]}) ->
		      package_info(RepoURL, Value)
              end, Rows).

package_info(RepoURL, RawMeta) ->
    {value, {_, NameBin}, Meta1} = lists:keytake(<<"name">>, 1, RawMeta),
    {value, {_, VsnBin}, Meta2} = lists:keytake(<<"version">>, 1, Meta1),
    {value, {_, ChecksumBin}, Meta3} = lists:keytake(<<"checksum">>, 1, Meta2),
    {value, {_, PackageType}, Meta4} = lists:keytake(<<"package_type">>, 1, Meta3),
    #package_info{name = binary_to_list(NameBin),
		  vsn = binary_to_list(VsnBin),
		  checksum = ChecksumBin,
		  timestamp = undefined,
		  repo = RepoURL,
		  package_type = binary_to_list(PackageType),
		  meta = Meta4
		 }.

construct_start_and_end_keys(SystemString, ExecutableEnv, PackageType, PackageMeta) ->
    Name = get_name(PackageMeta),
    lists:foldr(fun(<<"*">>, {S, E}) ->
                        {[0 | S], [{struct, []} | E]};
                   ({L, H}, {S, E}) ->
                        {[L | S], [H | E]};
                   (X, {S, E}) ->
                        {[<<"any">> | S], [X | E]}
                end, {[], []}, [SystemString, ExecutableEnv, PackageType, Name]).

get_couch_repo(Repo) when is_record(Repo, db) ->
    {ok, Repo};
get_couch_repo(RepoURL) when is_list(RepoURL) ->
    {http, Repo, Port} = parse(RepoURL),
    Server = couchbeam:server_connection(Repo, Port, "", []),
    couchbeam:open_db(Server, "packages").

add_package(RepoURL, PackageBin, Checksum, PackageInfo) ->
    #package_info{executable_env = _ExecutableEnv,
		  package_type   = PackageType,
		  name   = PackageName,
		  meta   = _PackageMeta} = PackageInfo,
    {ok, Repo} = get_couch_repo(RepoURL),

    case create_version(Repo, Checksum, PackageType, PackageInfo) of
        already_exists ->
            already_exists;
        Json ->
            {{<<"_id">>, ID}, {<<"_rev">>, NewRev}} = save_and_return_rev(Repo, Json),
            add_attachment(Repo, ID, PackageName, NewRev, PackageBin)
    end.

create_version(Repo, Checksum, <<"erts">>, PackageInfo) ->
    create_erts_version(Repo, Checksum, PackageInfo);
create_version(Repo, Checksum, <<"release">>, PackageInfo) ->
    create_rel_version(Repo, Checksum, PackageInfo);
create_version(Repo, Checksum, <<"target_system">>, PackageInfo) ->
    create_rel_version(Repo, Checksum, PackageInfo);
create_version(Repo, Checksum, _Type, PackageInfo) ->
    create_app_version(Repo, Checksum, PackageInfo).

create_erts_version(Repo, Checksum, PackageInfo) ->
    {match,[Version]} = re:run(PackageInfo#package_info.name, "erts-(.*)..tar.gz", [{capture, [1], list}]),
    VersionBinary = list_to_binary(Version),
    Name = <<"erts-", VersionBinary/binary>>,

    case version_exists(Repo, Version, PackageInfo) of
        false ->
            {[{<<"name">>, Name},
              {<<"version">>, VersionBinary},
              {<<"checksum">>, Checksum},
              {<<"package_type">>, PackageInfo#package_info.package_type},
              {<<"system">>, system_string(PackageInfo)},
              {<<"env">>, PackageInfo#package_info.executable_env}]};
        true ->
            already_exists
    end.

create_app_version(Repo, Checksum, PackageInfo) ->
    {Name, Version, Desc, Deps} = get_app_metadata(PackageInfo#package_info.meta),
    VersionBinary = list_to_binary(Version),

    case version_exists(Repo, Version, PackageInfo) of
        false ->
            {[{<<"name">>, Name},
              {<<"version">>, VersionBinary},
              {<<"checksum">>, Checksum},
              {<<"package_type">>, PackageInfo#package_info.package_type},
              {<<"system">>, system_string(PackageInfo)},
              {<<"env">>, PackageInfo#package_info.executable_env},
              {<<"desc">>, Desc},
              {<<"dependencies">>, Deps}]};
        true ->
            already_exists
    end.

create_rel_version(Repo, Checksum, PackageInfo) ->
    {Name, Version, Deps} = get_rel_metadata(PackageInfo#package_info.meta),
    VersionBinary = list_to_binary(Version),

    case version_exists(Repo, Version, PackageInfo) of
        false ->
            {[{<<"name">>, Name},
              {<<"version">>, VersionBinary},
              {<<"checksum">>, Checksum},
              {<<"package_type">>, PackageInfo#package_info.package_type},
              {<<"system">>, system_string(PackageInfo)},
              {<<"env">>, PackageInfo#package_info.executable_env},
              {<<"dependencies">>, Deps}]};
        true ->
            already_exists
    end.

system_string(#package_info{os_name = OSName, os_release = OSRelease,
				  hardware_name = HardwareName, glibc_vsn = undefined}) ->
    lists:concat([OSName, "--", OSRelease, "--", HardwareName]);
system_string(#package_info{os_name = OSName, os_release = OSRelease,
				  hardware_name = HardwareName, glibc_vsn = GlibC}) ->
    lists:concat([OSName, "--", OSRelease, "--", GlibC, "--", HardwareName]).

version_exists(Repo, Version, PackageInfo) ->
    case list(Repo, PackageInfo) of
        [] ->
            false;
        Rows ->
            lists:any(fun(Value) ->
                              Version == Value#package_info.vsn
                      end, Rows)
    end.

add_attachment(DB, ID, PackageName, Rev, File) ->
    try
        couchbeam:put_attachment(DB, ID, PackageName, File, [{rev, Rev}])
    catch
        _C:E ->
            throw(?UEX({failed_add_attachment, {E, PackageName}},
                       "Unable to attach ~p to ~p.",
                       [PackageName, File]))
    end.


save_and_return_rev(DB, Package) ->
    NewDoc = couchbeam_save_doc(DB, Package),
    {lists:keyfind(<<"_id">>, 1, NewDoc), lists:keyfind(<<"_rev">>, 1, NewDoc)}.

couchbeam_save_doc(DB, Package) ->
    try
        {ok, {NewDoc}} = couchbeam:save_doc(DB, Package),
        NewDoc
    catch
        _C:E ->
            throw(?UEX({save_doc_failure, {E, DB}},
                       "Failure to save doc in repo ~p~n. Check your connection or try with a new repo",
                       [DB]))
    end.


get_name([{release,
           {Name, _Version},
           {erts, _ErtsVsn},
           _Apps}]) ->
    list_to_binary(Name);
get_name([{application, Name, _List}]) ->
    list_to_binary(atom_to_list(Name));
get_name(Name) ->
    Name.

get_rel_metadata([Rel]) ->
    {release,
     {Name, Version},
     {erts, _ErtsVsn},
     Apps} = Rel,

    JsonApps = rel_apps_list_to_json(Apps),

    {list_to_binary(Name), Version, JsonApps}.

get_app_metadata([App]) ->
    {application, Name, _List} = App,
    {vsn, Version} = get_app_vsn(App),
    {description, Desc} = get_app_desc(App),
    {applications, Deps} = get_app_deps(App),

    JsonDeps = atom_list_to_json(Deps),

    {list_to_binary(atom_to_list(Name)), Version, list_to_binary(Desc), JsonDeps}.

get_app_vsn({application, _Name, List}) ->
    lists:keyfind(vsn, 1, List).

get_app_desc({application, _Name, List}) ->
    lists:keyfind(description, 1, List).

get_app_deps({application, _Name, List}) ->
    lists:keyfind(applications, 1, List).

atom_list_to_json(List) ->
    lists:map(fun(X) ->
                      list_to_binary(atom_to_list(X))
              end, List).

rel_apps_list_to_json(RelApps) ->
    lists:map(fun({Name, Version}) ->
                      {list_to_binary(atom_to_list(Name)), list_to_binary(Version)}
              end, RelApps).

parse([$h,$t,$t,$p,$:,$/,$/|T]) ->  parse_http(T);
parse(X) -> {http, X, 80}.

parse_http(X) ->
    case string:chr(X, $/) of
        0 ->
            parse_http(X ++ "/");
        N ->
            Host = string:substr(X, 1, N-1),
            case string:chr(Host, $:) of
                0 ->
                    Port = 80,
                    {http, Host, Port};
                M ->
                    Site = string:substr(Host,1,M-1),
                    case (catch list_to_integer(
                                  string:substr(Host, M+1, length(Host)))) of
                        {'EXIT', _} ->
                            {http, Site, 80};
                        Port ->
                            {http, Site, Port}
                    end
            end
    end.

