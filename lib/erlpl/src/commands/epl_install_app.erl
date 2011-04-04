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
-module(epl_install_app).

%% API
-export([run/2, error/1, spec/0, description/0]).

-include("erlpl.hrl").

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Install an application.
%% @spec (AppDir, Options) -> ok
%% where
%%  Options = [Option]
%%   Option = {force, bool()} 
%% @end
%%--------------------------------------------------------------------
run(RawApp, Options) ->
    AppDir = epl_util:unpack_to_tmp_if_archive(RawApp),
    ?DEBUG("Unpacked application to ~p~n", [AppDir]),
    Res = 
	case epl_validation:app_validation(AppDir) of
	    {error, Reason} ->
		Msg = "Validation for ~s failed with ~p.~n" ++
		    "Please verify that you have a well formed OTP Application~n",
		Vars = [AppDir, Reason],
		throw(?UEX({validation_of_app_failed, AppDir, Reason}, Msg, Vars));
	    true -> 
		epl_installed_info:add_managed_root_dir(Options),
		handle_install_app(AppDir, Options) 
	end,
    epl_file:remove(AppDir, [recursive]),
    Res.

error(_Error) ->
    "who knows what happened?~n".

description() ->
    "install an application package".

-spec spec() -> get_opts_spec().
spec() ->
    CmdLnTail = "<pkg_dir>",
    OptionsTail = [{"pkg_dir", "path to the application package to be installed"}],
    OptionSpecs =
	[
      %% {Name,   ShortOpt, LongOpt,        ArgSpec,               HelpMsg}
	 {verbose,     $v,  "verbose",     undefined,                "Verbose output"},
	 {root_dir,    $d,  "root_dir",     string,                "The root dir for the install"},
	 {version,     $n,  "version",      string,                "App version number"},
	 {force,       $f,  "force",        undefined,             "Forces the command to run and eliminates all prompts"}
	],
    {OptionSpecs, CmdLnTail, OptionsTail}.



%%%---------------------------------------------------------
%%% Internal Functions
%%%---------------------------------------------------------

handle_install_app(AppDir, Options) ->
    RootDir  = epl_util:get_option(root_dir, Options, spec(), required),
    {AppName, AppVsn} = epl_otp_metadata_lib:app_name_and_vsn(AppDir),
    InstalledAppDir   = epl_installed_paths:app_dir(RootDir, AppName, AppVsn),
    Fun = fun() ->
		  epl_installed_info:write(AppDir, Options),
		  epl_install_driver:install_app(AppDir, RootDir)
	  end,
    case filelib:is_dir(InstalledAppDir) of
	false ->
	   Fun();
	true  ->
	    Prompt = io_lib:fwrite("Do you want to overwrite ~s-~s (y/n)?", [AppName, AppVsn]), 
	    RespSet = ["y", "n"],
	    SuccessSet = ["y"],
	    epl_util:force_or_prompt_for_an_action(Prompt, RespSet, SuccessSet, Fun, Options) 
    end.

