%%% Types

%% Commandline option spec for getopts.
-type get_opts_spec() :: {string(), [tuple()], [tuple()]}.
-type option_list() :: [option()].
-type option() :: {atom(), term()} | atom().

%%% Record definitions

%% OS name can be something like "Linux" or "Darwin" run uname -s to see examples
%% The major and minor version numbers of your os release. See uname -r for the full num
%% Hardware name is found on unix like systems with uname -m an is typically something
%% like i386.
%% Executable environment is the type of environment an package runs in. This could
%% be native if it is C code but since we typically deal with Erlang packages this
%% is your erts version number. 
%% Package meta is a grabbag of various things depending on the package being published
%% for Erlang apps it is the .app term and for releases it is the .rel term.
-record(package_info, {name, vsn, os_name, os_release, hardware_name, glibc_vsn, executable_env, checksum, timestamp, repo, repo_type, root_dir, path, package_type, meta = []}).
			  

-define(RELEASE_PACKAGE_TYPE_IDS, [release, "release"]). 
-define(ERTS_PACKAGE_TYPE_IDS, [erts, "erts"]).
-define(APP_PACKAGE_TYPE_IDS, [app_binary_specific, app_binary_generic, app_source, "app--source", "app--binary"]). 
 

%%% Macro definitions
-define(REPO_CACHE, "repo_cache").
-define(LOCAL_PACKAGE_CACHE, "local_package_cache").

-define(CURRENT_FUNCTION, lists:flatten(io_lib:fwrite("~p:~p/~p", tuple_to_list( hd( tl( tuple_to_list( process_info(self(), current_function)))))))).

%% For formatting exceptions
-define(UEX(Exception, UMSG, UVARS),
	{uex, {?CURRENT_FUNCTION, ?LINE, Exception, lists:flatten(io_lib:fwrite(UMSG, UVARS))}}).
-define(EX(Exception), {ex, {?CURRENT_FUNCTION, ?LINE, Exception}}).
-define(THROW(Function, EX), try Function catch _:_ -> EX end).
-define(FORMAT(Format, Data), io:format("~s:~p " ++ Format, [?CURRENT_FUNCTION, ?LINE|Data])).

-define(LEVEL(Level), case Level of false -> 2;"0"->0;"1"->1;"2"->2;"3"->3;"4"->4;"5"->5; "off" -> 0;"debug" -> 1;"info" -> 2;"error" -> 3;"warn" -> 4;"fatal" -> 5 end).

-define(DEBUG(Format, Data),  case ?LEVEL(os:getenv("ERLP_LOG_LEVEL")) == 1 of true -> ?FORMAT(Format, Data); _ -> ok end).
-define(INFO(Format, Data),  case ?LEVEL(os:getenv("ERLP_LOG_LEVEL")) =< 2 of true -> io:format(Format, Data); _ -> ok end).
-define(ERROR(Format, Data),  case ?LEVEL(os:getenv("ERLP_LOG_LEVEL")) =< 3 of true -> ?FORMAT(Format, Data); _ -> ok end).
-define(WARN(Format, Data),  case ?LEVEL(os:getenv("ERLP_LOG_LEVEL")) =< 4 of true -> ?FORMAT(Format, Data); _ -> ok end).
-define(FATAL(Format, Data),  case ?LEVEL(os:getenv("ERLP_LOG_LEVEL")) =< 5 of true -> ?FORMAT(Format, Data); _ -> ok end).

-define(PACKAGE_NAME_REGEXP, "[a-z]+[a-zA-Z0-9_]*").
-define(PACKAGE_VSN_REGEXP, "[a-zA-Z0-9_]+([.-][a-zA-Z0-9_]+)*").
-define(REPO_FILE_EXT_REGEXP, "(\.tar\.gz|\.tgz|\.epkg|\.app)").
-define(PACKAGE_NAME_AND_VSN_REGEXP,
	        lists:flatten(["^", ?PACKAGE_NAME_REGEXP, "-", ?PACKAGE_VSN_REGEXP, ?REPO_FILE_EXT_REGEXP, "*$"])).


