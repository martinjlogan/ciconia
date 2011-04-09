%%%-------------------------------------------------------------------
%%% @author Martin Logan
%%% @copyright (C) 2010, Erlware
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
-module(ep_cmd_install_erts).

%% API
-export([run/2, spec/0, description/0]).

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
run(ErtsVsn, Options) ->
    epl_util:assert_option(root_dir, Options),
    epl_util:assert_option(timeout, Options),
    install_erts(ErtsVsn, Options).

description() ->
    "Install an Erlang ERTS (Erlang Runtime System) package".

-spec spec() -> get_opts_spec().
spec() ->
    CmdLnTail = "",
    OptionsTail = "",
    OptionSpecs =
	[
      %% {Name,   ShortOpt, LongOpt,        ArgSpec,           HelpMsg}
	 {verbose, $v, "verbose", undefined, "Option for verbose output"},
	 {root_dir,$d, "root_dir", string,"The root dir for the installation"},
	 {force, $f, "force", undefined,
	  "Forces the command to run and eliminates all prompts"},
	 {timeout, $t, "timeout", {integer, 60000},
	  "The timeout value for the operation"}
	],
    {OptionSpecs, CmdLnTail, OptionsTail}.

%%%===================================================================
%%% Internal Functions
%%%===================================================================
install_erts(ErtsVsn, Options) ->
    List = ep_cache:fetch("erts", ErtsVsn, erts, Options),
    ep_install_util:ep_cache_fetch_ensure("erts", List),
    Info = select_erts_to_install(List),
    ErtsBinary = ep_install_util:fetch_binary(Info, Options),
    ErtsPackagePath = ep_install_util:write_out_package(Info, ErtsBinary),
    epl_cmd_install_erts:run(ErtsPackagePath, Options).
    %epl_file:remove(ErtsPackagePath, [recursive]). 
    

select_erts_to_install([]) ->
    throw(?UEX(erts_not_found,
	       "The erts version you are trying to install can't be found~n" ++
	       "Please check your net connection. If that is working try~n" ++
	       "running erlp update-cache -a <repo-type> -r <repo-url>~n",
	       []));
select_erts_to_install([Info|_]) ->
    Info.

%%%===================================================================
%%% Test Functions
%%%===================================================================
