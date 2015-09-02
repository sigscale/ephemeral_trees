%%%---------------------------------------------------------------------
%%% @copyright 2014-2015 Motivity Telecom
%%% @author Vance Shipley <vances@motivity.ca> [http://www.motivity.ca]
%%% @end
%%%
%%% Copyright (c) 2014-2015, Vance Shipley
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
%%% @doc ephemeral_trees API tests
%%%--------------------------------------------------------------------
%%%
-module(ephemeral_trees_api_SUITE).

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
	[{timetrap, {minutes, 1}}].

%% @spec (Config) -> Config
%% 	Config = [tuple()]
%% @doc Initiation before the whole suite.
%%
init_per_suite(Config) ->
	Config.

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
	[new, insert, find, remove, expire, overwrite_item, overwrite_time].

%%---------------------------------------------------------------------
%%  Test cases
%%---------------------------------------------------------------------

new() ->
	[{userdata, [{doc, "Create new treap."}]}].

new(_Config) ->
	nil = ephemeral_trees:new().

insert() ->
	[{userdata, [{doc, "Insert example values used in paper."}]}].

insert(_Config) ->
	example_insert().

find() ->
	[{userdata, [{doc, "Find example values used in paper."}]}].

find(_Config) ->
	T = example_insert(),
	a = ephemeral_trees:find(T, 1),
	b = ephemeral_trees:find(T, 2),
	c = ephemeral_trees:find(T, 3),
	d = ephemeral_trees:find(T, 4),
	e = ephemeral_trees:find(T, 5),
	f = ephemeral_trees:find(T, 6),
	g = ephemeral_trees:find(T, 7).

remove() ->
	[{userdata, [{doc, "Remove example values used in paper."}]}].

remove(_Config) ->
	T = example_insert(),
   {{{nil,1,7,a,nil},3,6,c,nil},2,6,b,{{nil,5,7,e,nil},6,6,f,{nil,7,8,g,nil}}} = ephemeral_trees:remove(T, 4).

expire() ->
	[{userdata, [{doc, "Expire example values used in paper."}]}].

expire(_Config) ->
	T = example_insert(),
	{nil,1,7,a,{nil,5,7,e,{nil,7,8,g,nil}}} = ephemeral_trees:expire(T, 6).

overwrite_item() ->
	[{userdata, [{doc, "Insert new item for existing key."}]}].

overwrite_item(_Config) ->
	T = example_insert(),
	{{{nil,1,7,a,nil},2,6,b,{nil,3,6,cbis,nil}},4,0,d,{{nil,5,7,e,nil},6,6,f,{nil,7,8,g,nil}}} = ephemeral_trees:insert(T, 3, 6, cbis).

overwrite_time() ->
	[{userdata, [{doc, "Insert new time for existing key."}]}].

overwrite_time(_Config) ->
	T = example_insert(),
	{{{nil,1,7,a,nil},2,6,b,{nil,3,8,c,nil}},4,0,d,{{nil,5,7,e,nil},6,6,f,{nil,7,8,g,nil}}} = ephemeral_trees:insert(T, 3, 8, c).

%%---------------------------------------------------------------------
%%  Internal functions
%%---------------------------------------------------------------------

example_insert() ->
	T1 = ephemeral_trees:insert(ephemeral_trees:new(), 1, 7, a),
	{nil,1,7,a,nil} = T1,
	T2 = ephemeral_trees:insert(T1, 2, 6, b),
	{{nil,1,7,a,nil},2,6,b,nil} = T2,
	T3 = ephemeral_trees:insert(T2, 3, 6, c),
	{{nil,1,7,a,nil},2,6,b,{nil,3,6,c,nil}} = T3,
	T4 = ephemeral_trees:insert(T3, 4, 0, d),
	{{{nil,1,7,a,nil},2,6,b,{nil,3,6,c,nil}},4,0,d,nil} = T4,
	T5 = ephemeral_trees:insert(T4, 5, 7, e),
	T6 = ephemeral_trees:insert(T5, 6, 6, f),
	T7 = ephemeral_trees:insert(T6, 7, 8, g),
	{{{nil,1,7,a,nil},2,6,b,{nil,3,6,c,nil}},4,0,d,{{nil,5,7,e,nil},6,6,f,{nil,7,8,g,nil}}} = T7.

