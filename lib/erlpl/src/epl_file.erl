%%%-------------------------------------------------------------------
%%% @author Martin Logan <martinjlogan@Macintosh.local>
%%% @copyright (C) 2011, Martin Logan
%%% @doc
%%%  Contains function that do file IO all nice and wrapped for
%%%  exception throwing. 
%%% @end
%%% Created : 14 Feb 2011 by Martin Logan <martinjlogan@Macintosh.local>
%%%-------------------------------------------------------------------
-module(epl_file).

%% API
-export([
	 copy/2,
	 copy/3,
	 write/2,
	 write_term/2,
	 read/1,
	 remove/1,
	 remove/2,
	 consult/1
	]).

-include("erlpl.hrl").

%%%===================================================================
%%% API
%%%===================================================================

%% @doc copy one file to another.
-spec copy(From::string(), To::string()) -> ok. 
copy(From, To) ->
    copy(From, To, []).

%% @doc copy one file or dir to another.
-spec copy(From::string(), To::string(), Options::list()) -> ok. 
copy(From, To, Options) ->
    try
	ok = ewl_file:copy(From, To, Options)
    catch
	_C:E ->
	    throw(?UEX({failed_to_copy, E},
		       "Could not copy the file or directory at~n~p~nto~n~p~n" ++
		       check_perms_msg(),
		       [From, To]))
    end.
		       
%% @doc delete a dir and throw an exception if it fails. 
-spec remove(Path::string()) -> no_return().
remove(Path) ->
    remove(Path, []).

%% @doc delete a dir and throw an exception if it fails. 
-spec remove(Path::string(), Options::list()) -> no_return().
remove(Path, Options) ->
    try
	ewl_file:remove(Path, Options)
    catch
	_:_ ->
	    throw(?UEX({remove_failed, Path},
		       "Failed to delete the file or directory at ~p~n" ++ 
		       check_perms_msg(),
		      [Path]))
    end.
    

%% @doc consult an erlang term file from the file system.
%%      Provide user readible exeption on failure.
-spec consult(FilePath::string()) -> term().
consult(FilePath) ->
    case file:consult(FilePath) of
	{ok, [Term]} ->
	    Term;
	{error, Error} ->
	    Msg = "The file at ~p~n" ++
		  "is either not a valid Erlang term, does not to exist~n" ++
		  "or you lack the permissions to read it. Please check~n" ++
		  "to see if the file exists and that it has the correct permissions~n",
	    throw(?UEX({failed_to_consult_file, {FilePath, Error}}, Msg, [FilePath]))
    end.

%% @doc read a file from the file system. Provide UEX exeption on failure.
-spec read(FilePath::string()) -> binary().
read(FilePath) ->
    try
	{ok, FileBin} = file:read_file(FilePath),
	FileBin
    catch
	_C:E -> throw(?UEX({read_failed, {FilePath, E}},
			   "Read failed for the file ~p with ~p~n" ++
			   check_perms_msg(),
			   [FilePath, E]))
    end.


%% @doc write a file to the file system. Provide UEX exeption on failure.
-spec write(FileName::string(), Contents::string()) -> ok.
write(FileName, Contents) ->
    case file:write_file(FileName, Contents) of
	ok ->
	    ok;
	{error, Reason} ->
	    Msg = "Writing the file ~s to disk failed with reason ~p.~n" ++
		"This file is required.~n" ++
		check_perms_msg(),
	    throw(?UEX({write_file_failure, {FileName, Reason}}, Msg, [FileName, Reason]))
    end.

%% @doc write a term out to a file so that it can be consulted later.
-spec write_term(string(), term()) -> ok.
write_term(FileName, Term) ->
    write(FileName, lists:flatten(io_lib:fwrite("~p. ", [Term]))).

%%%===================================================================
%%% Internal functions
%%%===================================================================
check_perms_msg() ->
    "Please check that you have the correct permissions and try again~n".
    
