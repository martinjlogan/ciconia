%%%-------------------------------------------------------------------
%%% @author Martin Logan <martinjlogan@Macintosh.local>
%%% @copyright (C) 2010, Martin Logan
%%% @doc
%%%  a library of code that is os specific.
%%% @end
%%% Created :  6 Jul 2010 by Martin Logan <martinjlogan@Macintosh.local>
%%%-------------------------------------------------------------------
-module(epl_os_specific).

%% API
-export([set_executable/1,
	 set_all_executable/1,
	 hardware_name/0,
	 os_release/0,
	 glibc_version/0,
	 os_name/0
	 ]).

-include("erlpl.hrl").

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Return the local OS name. Throws an exception for unsupported
%%      OS types. Examples of the return value for this are
%%      "Linux" and "Darwin".
-spec os_name() -> string().
os_name() ->
    %% TODO - figure out how to check on this one.
    strip_n(os:cmd("uname -s")).

%% @doc Return the local glibc version. 
-spec glibc_version() -> string().
glibc_version() ->
    case catch os:cmd("/lib/libc.so.6 | head -n 1 | perl -pe 's|.*?([0-9]*\.[0-9]*\.?[0-9]*).*|\1|'") of
	[H|_] = Glibc when is_integer(H) ->
	    strip_n(Glibc);
	Error ->
	    throw(?EX({failed_to_find_glibc_version, Error}))
    end.

%% @doc Return the local OS release. Throws an exception for unsupported
%%      OS types.
-spec os_release() -> string().
os_release() ->
    RelNum = re:replace(strip_n(os:cmd("uname -r")), "(\\d*\\.\\d*).*", "\\1", [{return, list}]),
    case re:run(RelNum, "\\d*\\.\\d*") of
	{match, [{0, Length}]} when Length == length(RelNum) ->
	    RelNum;
	_Error ->
	    throw(?UEX({cant_parse_os_release_vsn, RelNum}, "Can't parse your OS version ~p. Please report this to Erlware.",
		       [RelNum]))
    end.

%% @doc Return the local hardware name.
-spec hardware_name() -> string().
hardware_name() ->
    strip_n(os:cmd("uname -m")).
    
%% @doc Applies executable permissions to the file provided. 
%% @spec set_executable(FileName) -> ok
set_executable(FileName) ->
    case erlang:system_info(system_architecture) of
	"win32" ->
	    ok;
	_SysArch ->
	    case catch os:cmd("chmod a+x " ++ FileName) of
		[]    ->
		    ok;
		Error ->
		    throw(?EX({set_executable_permission_failed, FileName, Error}))
	    end
    end.

%% @doc Applies executable permissions recursively to all files in the directory.
%% @spec (Dir) -> ok 
set_all_executable(Dir) ->
    case erlang:system_info(system_architecture) of
	"win32" ->
	    ok;
	_SysArch ->
	    case catch os:cmd("chmod -R a+x " ++ Dir) of
		[]    ->
		    ok;
		Error ->
		    throw(?EX({set_executable_permission_failed, Dir, Error}))
	    end
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

strip_n(String) ->
    case lists:reverse(String) of
	[$\n|T] -> lists:reverse(T);
	String  -> String
    end.
	    

    

