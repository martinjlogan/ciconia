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
-module(ep_update_cache).

%% API
-export([run/1, spec/0, description/0]).

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
run(Options) ->
    cache_package_list(Options).
    
description() ->
    "Update the cache of packages".

-spec spec() -> get_opts_spec().
spec() ->
    CmdLnTail = "",
    OptionsTail = "",
    OptionSpecs =
	[
      %% {Name,   ShortOpt, LongOpt,        ArgSpec,           HelpMsg}
	 {repo_type, $a, "repo_type", atom,
	  "specifiy the repo type here. \"faxien\" | \"dav\" | \"couchdb\" | \"agner\""},
	 {verbose, $v, "verbose", undefined,  "Option for verbose output"},
	 {repos,   $r, "repos", {string, "http://repo.erlware.org/pub"},
	  "The repos to search"},
	 {timeout, $t, "timeout", {integer, 60000},
	  "The timeout value for the operation"}
	],
    {OptionSpecs, CmdLnTail, OptionsTail}.

%%%===================================================================
%%% Internal Functions
%%%===================================================================

cache_package_list(Options) ->
    epl_util:assert_option(repo_type, Options, spec()),
    epl_util:assert_option(meta_dir, Options),
    Repos = epl_util:get_option(repos, Options, spec(), required),
    Timeout = epl_util:get_option(timeout, Options, spec(), required),
    Driver = ep_util:driver(Options),
    SearchCriteria = 
	[generic_search_criteria(),
	 os_specific_search_criteria(epl_os_specific:os_name())],
    PackageInfoList = Driver:list(Repos, SearchCriteria, Timeout),
    ep_cache:write(PackageInfoList, Options).

generic_search_criteria() ->
    package_search_criteria(undefined, undefined, undefined,
			    undefined, "erts-*", "app | release | erts").

os_specific_search_criteria("Darwin" = OSName) ->
    OSRelease = epl_os_specific:os_release(),
    HardwareName = epl_os_specific:hardware_name(),
    [Major, Minor] = string:tokens(OSRelease, "."),
    OSReleaseRange = Major ++ "." ++ create_range(Minor, "0"),
    package_search_criteria(OSName, OSReleaseRange, HardwareName,
			    undefined, "erts-*", "app | release | erts");
os_specific_search_criteria("Linux" = OSName) ->
    OSRelease = epl_os_specific:os_release(),
    GLibC = epl_os_specific:glibc_version(),
    HardwareName = epl_os_specific:hardware_name(),
    [Major, Minor] = string:tokens(OSRelease, "."),
    [MajorGL, MinorGL] = string:tokens(GLibC, "."),
    OSReleaseRange = Major ++ "." ++ create_range(Minor, "0"),
    GlibCRange = MajorGL ++ "." ++ create_range(MinorGL, "0"),
    package_search_criteria(OSName, OSReleaseRange, HardwareName,
			    GlibCRange, "erts-*", "app | release | erts").

create_range(High, Low) when is_list(High), is_list(Low) ->
    "[" ++ High ++ "-" ++ Low ++ "]". 

package_search_criteria(OSName, OSRelease, HardwareName, GLibC, ErtsVsn, PackageType) ->
    #package_info{os_name = OSName,
		  os_release = OSRelease,
		  hardware_name = HardwareName,
		  glibc_vsn = GLibC,
		  executable_env = ErtsVsn,
		  package_type = PackageType}.
