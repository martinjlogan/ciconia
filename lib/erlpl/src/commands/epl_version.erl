%%-------------------------------------------------------------------
%%% @author Martin Logan <martinjlogan@Macintosh.local>
%%% @copyright (C) 2010, Martin Logan
%%% @doc
%%%  Print the version of erlpl
%%% @end
%%% Created : 16 Jun 2010 by Martin Logan <martinjlogan@Macintosh.local>
%%%-------------------------------------------------------------------
-module(epl_version).

%% API
-export([run/1, error/1, spec/0, description/0]).

-include("erlpl.hrl").

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc dummy command
%% @end
%%--------------------------------------------------------------------
run(_Arg) ->
    unimplemented.

error(_Error) ->
    "who knows what happened?~n".

description() ->
    "print the current erlpl version".

-spec spec() -> get_opts_spec().
spec() ->
    OptionSpecs =
	[
      %% {Name,     ShortOpt,  LongOpt,        ArgSpec,               HelpMsg}
	],
    {OptionSpecs, "", ""}.

