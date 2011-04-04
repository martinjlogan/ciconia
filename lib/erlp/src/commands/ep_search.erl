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
-module(ep_search).

%% API
-export([run/2, error/1, spec/0, description/0]).
-export([print_installed/1]).

-include("erlpl.hrl").
-include("eunit.hrl").

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc List installed packages.
%% @spec (Options) -> ok
%% where
%%  Options = [Option]
%%   Option = {force, bool()} 
%% @end
%%--------------------------------------------------------------------
run(SearchTerm, Options) ->
    SearchType = epl_util:get_val(type, Options),
    ?DEBUG("the search type is ~p and searching ~p~n", [SearchType, SearchTerm]),
    Results = 
	case SearchType of
	    text ->
		ep_cache:fetch(SearchTerm, Options);
	    regexp ->
		throw(?UEX(unimplemented, "regexp search is not yet implemented", []))
	end,
    io:format("results ~p~n", [Results]).
    


error(_Error) ->
    "who knows what happened?~n".

description() ->
    "list packages".

-spec spec() -> get_opts_spec().
spec() ->
    CmdLnTail = "",
    OptionsTail = "",
    OptionSpecs =
	[
      %% {Name,   ShortOpt, LongOpt,        ArgSpec,           HelpMsg}
	 %{repo_type, $a,      "repo_type",  string,
	 % "specifiy the repo type here. \"faxien\" | \"dav\" | \"couchdb\" | \"agner\""},
	 {type,   $t,      "type",      {atom, text},  "Option for verbose output: text or regexp"},
	 {verbose, $v,      "verbose",      undefined,  "Option for verbose output"}
	],
    {OptionSpecs, CmdLnTail, OptionsTail}.

%%--------------------------------------------------------------------
%% @doc used to format the output of the list functions
%% @end
%%--------------------------------------------------------------------
print_installed(NameVsnPairs) ->
    NameVsnsPairs = collect_dups(sort_name_and_vsn_pairs(NameVsnPairs)),
    L1 = [{Name,format_vsns(lists:reverse(Vsns))} || {Name, Vsns} <- NameVsnsPairs],
    Col = lists:foldr(fun ({A,_B},Max) when length(A)>Max ->
			      length(A);
			  (_, Max) ->
			      Max
		      end, 0, L1),
    Fmt = lists:flatten(io_lib:format("~s-~ps~s~n",["~",Col+3,"~s"])),
    lists:foreach(fun(T) -> ?INFO(Fmt, tuple_to_list(T)) end, L1).

%%%---------------------------------------------------------
%%% Internal Functions
%%%---------------------------------------------------------
pretty_print_app(App, Rel, RootDir) when App =:= true; App =:= undefined andalso Rel =:= undefined ->
    ?INFO("~nApplications Installed:~n", []),
    ?INFO("-----------------------~n", []),
    NameVsnPairs = list_name_and_vsn_dir(epl_installed_paths:lib_dir(RootDir)),
    print_installed(NameVsnPairs);
pretty_print_app(_App, _Rel, _RootDir) ->
    ok.
    
pretty_print_release(App, Rel, RootDir) when Rel =:= true; App =:= undefined andalso Rel =:= undefined ->
    ?INFO("~nReleases Installed:~n", []),
    ?INFO("-------------------~n", []),
    NameVsnPairs = list_name_and_vsn_dir(epl_installed_paths:releases_dir(RootDir)),
    print_installed(NameVsnPairs);
pretty_print_release(_App, _Rel, _RootDir) ->
    ok.

list_name_and_vsn_dir(Dir) ->
    lists:reverse(
      ordsets:to_list(
	ordsets:from_list(
	  epl_util:dedupe_tuple_list(
	    2, 
	    lists:sort(
	      fun({N, V}, {N, V1}) -> ewr_util:is_version_greater(V, V1);
		 ({N, _}, {N1, _}) -> N > N1 end,
	      name_and_vsn_tuples(filelib:wildcard(Dir ++ "/*"))
	     ))))).
    
%% Return a list of name and vsn tuples
name_and_vsn_tuples(Paths) ->
    lists:foldl(fun(Path, Acc) ->
			case catch epl_otp_metadata_lib:package_dir_to_name_and_vsn(Path) of
			    {Name, Vsn} -> [{Name, Vsn}|Acc];
			    _           -> Acc
			end
		end, [], Paths).



format_vsns(Vsns) when length(Vsns) > 5 ->
    SortedVsns = lists:sort(fun(V1, V2) -> ewr_util:is_version_greater(V1, V2) end, Vsns),
    lists:flatten([ewr_util:join(lists:reverse(lists:nthtail(length(Vsns) - 5, lists:reverse(SortedVsns))), " | "), " | ..."]);
format_vsns(Vsns) ->
    SortedVsns = lists:sort(fun(V1, V2) -> ewr_util:is_version_greater(V1, V2) end, Vsns),
    ewr_util:join(SortedVsns, " | ").

collect_dups([]) -> 
    [];
collect_dups([{Name, Vsn}|NameAndVsnPairs]) -> 
    collect_dups(NameAndVsnPairs, [{Name, [Vsn]}]).

collect_dups([{Name, Vsn}|T], [{Name, Vsns}|Acc]) ->
    collect_dups(T, [{Name, [Vsn|Vsns]}|Acc]);
collect_dups([{Name, Vsn}|T], Acc) ->
    collect_dups(T, [{Name, [Vsn]}|Acc]);
collect_dups([], Acc) ->
    Acc.

sort_name_and_vsn_pairs(NameAndVsnPairs) ->
    lists:sort(
      fun({N, V}, {N, V1}) -> ewr_util:is_version_greater(V, V1);
	 ({N, _}, {N1, _}) -> N > N1
      end,
      NameAndVsnPairs).

