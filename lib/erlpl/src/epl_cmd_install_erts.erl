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
-module(epl_cmd_install_erts).

%% API
-export([run/2, spec/0, description/0]).

-include("erlpl.hrl").

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Install an ertslication.
%% @spec (ErtsDir, Options) -> ok
%% where
%%  Options = [Option]
%%   Option = {force, bool()} 
%% @end
%%--------------------------------------------------------------------
run(RawErts, Options) ->
    ErtsDir = epl_util:unpack_to_tmp_if_archive(RawErts),
    Res = 
	case epl_validation:erts_validation(ErtsDir) of
	    {error, Reason} ->
		Msg = "Validation for ~s failed with ~p.~n" ++
		    "Please verify that you have a well formed ERTS package~n",
		Vars = [ErtsDir, Reason],
		throw(?UEX({validation_of_erts_failed, ErtsDir, Reason},
			   Msg,
			   Vars));
	    true -> 
		epl_installed_info:add_managed_root_dir(Options),
		handle_install_erts(ErtsDir, Options) 
	end,
    epl_file:remove(ErtsDir, [recursive]),
    Res.

description() ->
    "Install an Erlang ERTS (Erlang Runtime System) package".

-spec spec() -> get_opts_spec().
spec() ->
    CmdLnTail = "<pkg_dir>",
    OptionsTail = [{"pkg_dir", "path to the ERTS package to be installed"}],
    OptionSpecs =
	[
      %% {Name,   ShortOpt, LongOpt,        ArgSpec,               HelpMsg}
	 {verbose, $v, "verbose", undefined, "Verbose output"},
	 {root_dir, $d, "root_dir", string, "The root dir for the install"},
	 {force, $f, "force", undefined,
	  "Forces the command to run and eliminates all prompts"}
	],
    {OptionSpecs, CmdLnTail, OptionsTail}.



%%%---------------------------------------------------------
%%% Internal Functions
%%%---------------------------------------------------------

handle_install_erts(ErtsDir, Options) ->
    RootDir  = epl_util:get_option(root_dir, Options, spec(), required),
    {"erts", ErtsVsn} =
	epl_otp_metadata_lib:package_dir_to_name_and_vsn(ErtsDir),
    InstalledErtsDir   = epl_installed_paths:erts_dir(RootDir, ErtsVsn),
    Fun = fun() ->
		  epl_installed_info:write(ErtsDir, Options),
		  epl_install_driver:install_erts(ErtsDir, RootDir)
	  end,
    case filelib:is_dir(InstalledErtsDir) of
	false ->
	   Fun();
	true  ->
	    Prompt = io_lib:fwrite("Do you want to overwrite erts-~s (y/n)?", [ErtsVsn]), 
	    RespSet = ["y", "n"],
	    SuccessSet = ["y"],
	    epl_util:force_or_prompt_for_an_action(Prompt, RespSet, SuccessSet, Fun, Options) 
    end.

