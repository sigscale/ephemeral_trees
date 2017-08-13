%%%---------------------------------------------------------------------
%%% @copyright 2014-2017 SigScale Global Inc.
%%% @author Vance Shipley <vances@sigscale.org> [http://www.sigscale.org]
%%% @end
%%%
%%% Copyright (c) 2014-2017, SigScale Global Inc.
%%% All rights reserved.
%%%
%%% Redistribution and use in source and binary forms, with or without
%%% modification, are permitted provided that the following conditions
%%% are met:
%%%
%%% 1. Redistributions of source code must retain the above copyright
%%% notice, this list of conditions and the following disclaimer.
%%%
%%% 2. Redistributions in binary form must reproduce the above copyright
%%% notice, this list of conditions and the following disclaimer in the
%%% documentation and/or other materials provided with the distribution.
%%%
%%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
%%% "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
%%% LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
%%% A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
%%% HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
%%% SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
%%% LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
%%% DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
%%% THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
%%% (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
%%% OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
%%%
%%%---------------------------------------------------------------------
%%% @doc ephemeral_trees load tests
%%%--------------------------------------------------------------------
%%%
-module(ephemeral_trees_load_SUITE).

%% common_test required callbacks
-export([suite/0, sequences/0, all/0]).
-export([init_per_suite/1, end_per_suite/1]).
-export([init_per_testcase/2, end_per_testcase/2]).

-compile(export_all).

-include("ct.hrl").

%%---------------------------------------------------------------------
%%  Test server callback functions
%%---------------------------------------------------------------------

%% @spec () -> DefaultData
%% 	DefaultData = [tuple()]
%% @doc Require variables and set default values for the suite.
%%
suite() ->
	case ct:get_config(runtime) of
		undefined ->
			[];
		RunTime ->
			[{timetrap, {seconds, RunTime + 60}}]
	end.

%% @spec (Config) -> Config
%% 	Config = [tuple()]
%% @doc Initiation before the whole suite.
%%
init_per_suite(Config) ->
	random:seed(os:timestamp()),
	case ct:get_config(runtime) of
		undefined ->
			Config;
		RunTime ->
			[{runtime, RunTime} | Config]
	end.

%% @spec (Config) -> any()
%% 	Config = [tuple()]
%% @doc Cleanup after the whole suite.
%%
end_per_suite(Config) ->
	Config.

%% @spec (TestCase, Config) -> Config
%% 	Config = [tuple()]
%% @doc Initiation before each test case.
%%
init_per_testcase(_TestCase, Config) ->
	Config.

%% @spec (TestCase, Config) -> any()
%% 	Config = [tuple()]
%% @doc Cleanup after each test case.
%%
end_per_testcase(_TestCase, _Config) ->
	ok.

%% @spec () -> Sequences
%% 	Sequences = [{SeqName, Testcases}]
%% 	SeqName = atom()
%% 	Testcases = [atom()]
%% @doc Group test cases into a test sequence.
%%
sequences() ->
	[].

%% @spec () -> TestCases
%% 	TestCases = [Case]
%% 	Case = atom()
%% @doc Returns a list of all test cases in this test suite.
%%
all() ->
	[load10k, load50k, load100k].

%%---------------------------------------------------------------------
%%  Test cases
%%---------------------------------------------------------------------

load10k() ->
	[{require, runtime},
			{userdata, [{doc, "Load a treap at ~10K/s for an hour."}]}].

load10k(Config) ->
	load_test(Config, 10000).

load50k() ->
	[{require, runtime},
			{userdata, [{doc, "Load a treap at ~50K/s for an hour."}]}].

load50k(Config) ->
	load_test(Config, 50000).

load100k() ->
	[{require, runtime},
			{userdata, [{doc, "Load a treap at ~100K/s for an hour."}]}].

load100k(Config) ->
	load_test(Config, 100000).

%%---------------------------------------------------------------------
%%  Internal functions
%%---------------------------------------------------------------------

load_test(Config, Rate) ->
	DataDir = ?config(data_dir, Config),
	PrivDir = ?config(priv_dir, Config),
	RunTime = ?config(runtime, Config),
	StartTime = erlang:system_time(),
	TimeUnit =  erlang:convert_time_unit(1, second, native),
	random:seed(os:timestamp()),
	EndTime = StartTime + RunTime * TimeUnit,
	Msg = io_lib:fwrite("This load test will run for ~b seconds.", [RunTime]),
	erlang:display(lists:flatten(Msg)),
	{ok, HtmlBinIn} = file:read_file(DataDir ++ "/load-test.html"),
	HtmlFile = "load-test-" ++ integer_to_list(Rate) ++ ".html",
	HtmlBinOut = re:replace(HtmlBinIn, "%load-test%",
			"load-test-" ++ integer_to_list(Rate)),
	ok = file:write_file(PrivDir ++ HtmlFile, HtmlBinOut),
	{ok, IoDevice} = file:open(PrivDir ++ "load-test-"
			++ integer_to_list(Rate) ++ ".csv", [write, exclusive]),
	ok = io:fwrite(IoDevice, "Time,Size,Operation,Units~n", []),
	Url = filename:basename(PrivDir) ++ "/" ++ HtmlFile,
	load_test(IoDevice, Url, Rate, TimeUnit,
			StartTime, EndTime, 0, ephemeral_trees:new()).

load_test(IoDevice, Url, Rate, TimeUnit, StartTime, EndTime, Inserts, Tree) ->
	case erlang:system_time() of
		Time when Time >= EndTime ->
			file:close(IoDevice),
			{comment, "<a href=\"" ++ Url ++ "\">Graph</a>"};
		Time when Inserts > Rate ->
			ExpireTime = Time - 60 * TimeUnit,
			{TC, NewTree} = timer:tc(ephemeral_trees, expire, [Tree, ExpireTime]),
			Now = erlang:system_time(),
			Second = (Now - StartTime) div TimeUnit,
			ok = io:fwrite(IoDevice, "~b,~b,clean,~b~n",
					[Second, erlang:external_size(Tree), TC div 1000]),
			load_test(IoDevice, Url, Rate, TimeUnit,
					StartTime, EndTime, 0, NewTree);
		Time ->
			NewInserts = random:uniform(Rate + Rate div 3),
			NewTree = load_loop(Time, NewInserts, Tree),
			Now = erlang:system_time(),
			Second = (Now - StartTime) div TimeUnit,
			ok = io:fwrite(IoDevice, "~b,~b,insert,~b~n",
					[Second, erlang:external_size(Tree), NewInserts]),
			sleep(TimeUnit, Now - Time),
			load_test(IoDevice, Url, Rate, TimeUnit, StartTime, EndTime,
					Inserts + NewInserts, NewTree)
	end.

load_loop(_, 0, Tree) ->
	Tree;
load_loop(Time, N, Tree) ->
	NewTree = ephemeral_trees:insert(Tree,
			random:uniform(999999999), Time, Time),
	load_loop(Time, N - 1, NewTree).

sleep(TimeUnit, Offset) when (Offset div TimeUnit) > 0 ->
	ok;
sleep(TimeUnit, Offset) ->
	Timeout = 1000 - Offset div (TimeUnit div 1000),
	receive
	after Timeout ->
		ok
	end.

