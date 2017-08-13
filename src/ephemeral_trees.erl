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
%%% @reference <a href="http://arxiv.org/pdf/cs/0505038v1.pdf">
%%% 	Efficient Management of Short-Lived Data</a>
%%% @todo Expire during operations?
%%%---------------------------------------------------------------------
%%%
-module(ephemeral_trees).

-export([new/0, find/2, insert/3, insert/4, remove/2, expire/2]).

-opaque treap() :: nil | {Left :: treap(), Key :: term(),
		Time :: erlang:system_time(), Item :: term(), Right :: treap()}.
-export_type([treap/0]).

-spec new() -> treap().
%% @doc Returns a new empty `Tree'.
new() ->
	nil.

-spec find(Tree :: treap(), Key :: term()) -> Item :: term().
%% @doc Find value `Item' with key `Key' in `Tree'.
find({_, Key, _, Item, _} = _Tree, Key) ->
	Item;
find({Left, K, _, _, _}, Key) when Key < K ->
	find(Left, Key);
find({_, _, _, _, Right}, Key) ->
	find(Right, Key).

-spec insert(Tree :: treap(), Key :: term(), Item :: term()) -> treap().
%% @equiv insert(Tree, Key, erlang:system_time(), Item)
insert(Tree, Key, Item) ->
	insert(Tree, Key, erlang:system_time(), Item).

-spec insert(Tree :: treap(), Key :: term(),
		Time :: erlang:system_time(), Item :: term()) -> treap().
%% @doc Inserts `Key' with value `Item' into `Tree'.
%% 	Returns a new `Tree'.
insert({Left, Key, _, _, Right} = _Tree, Key, Time, Item) ->
	{Left, Key, Time, Item, Right};
insert({Left, K, T, I, Right}, Key, Time, Item) when Key < K ->
	rebalance({insert(Left, Key, Time, Item), K, T, I, Right});
insert({Left, K, T, I, Right}, Key, Time, Item) ->
	rebalance({Left, K, T, I, insert(Right, Key, Time, Item)});
insert(nil, Key, Time, Item) ->
	{nil, Key, Time, Item, nil}.

-spec remove(Node :: treap(), Key :: term()) -> treap().
%% @doc Remove `Key' from `Tree'.
remove({_, Key, _, _, _} = Node, Key) ->
	percolate(Node);
remove({Left, K, T, I, Right}, Key) when Key < K ->
	{remove(Left, Key), K, T, I, Right};
remove({Left, K, T, I, Right}, Key) ->
	{Left, K, T, I, remove(Right, Key)}.
	
-spec expire(Node :: treap(), Time :: erlang:system_time()) -> treap().
%% @doc Remove everything older than `Time'.
expire({_, K, T, _, _} = Node, Time) when T < Time ->
	expire(remove(Node, K), Time);
expire(Node, _Time) ->
	Node.

%%%
%%% internal functions
%%% 

-spec rebalance(Node :: treap()) -> treap().
%% @doc Rebalance through order-preserving node rotations.
%% @private
rebalance({{S1, K, T, I, S2}, Key, Time, Item, S3} = _Node) when Time > T ->
	{S1, K, T, I, {S2, Key, Time, Item, S3}};
rebalance({S1, Key, Time, Item, {S2, K, T, I, S3}}) when Time > T ->
	{{S1, Key, Time, Item, S2}, K, T, I, S3};
rebalance(Node) ->
	Node.

-spec percolate(Node :: treap()) -> treap().
%% @doc Rebalancing node rotations during removal.
%% @private
percolate({nil, _, _, _, nil}) ->
	nil;
percolate({nil, K, T, I, {Lr, Kr, Tr, Ir, Rr}}) ->
	{Lr, Kr, Tr, Ir, percolate({nil, K, T, I, Rr})};
percolate({{Ll, Kl, Tl, Il, Rl}, K, T, I, nil}) ->
	{percolate({Ll, K, T, I, nil}), Kl, Tl, Il, Rl};
percolate({{Ll, Kl, Tl, Il, Rl}, K, T, I,
		{_, _, Tr, _, _} = Right}) when Tl =< Tr ->
	{percolate({Ll, K, T, I, Rl}), Kl, Tl, Il, Right};
percolate({{_, _, _, _, _} = Left, K, T, I, {Lr, Kr, Tr, Ir, Rr}}) ->
	{Left, Kr, Tr, Ir, percolate({Lr, K, T, I, Rr})}.

