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
%%% @reference <a href="http://arxiv.org/pdf/cs/0505038v1.pdf">
%%% 	Efficient Management of Short-Lived Data</a>
%%% @todo Expire during operations?
%%% @end
%%%---------------------------------------------------------------------
%%%
-module(ephemeral_trees).

-export([new/0, find/2, insert/3, insert/4, remove/2, expire/2]).

-opaque treap() :: undefined | {Left :: treap(), Key :: term(),
		Time :: pos_integer(), Item :: term(), Right :: treap()}.
-export_type([treap/0]).
%% @type treap(). The treap data structure is a combination of a tree
%% 	and a heap. This `treap()' is a binary search tree with respect
%% 	to a `Key' and a heap with respect to an integer `Time'. The
%% 	tree is rebalanced when an `Item' is inserted or removed so that
%%		the oldest entries are leef nodes and the newest are clustered
%% 	around the root of the tree. The main benefit is that expiring
%% 	all entries older than a selected `Time' may be done very
%% 	efficiently by pruning branches.

-spec new() -> Tree
	when
		Tree :: treap().
%% @doc Returns a new empty `Tree'.
new() ->
	undefined.

-spec find(Tree, Key) -> Item
	when
		Tree :: treap(),
		Key :: term(),
		Item :: term().
%% @doc Retrieves the value stored with `Key' in `Tree'.
%% 	Assumes the key exists, crashes otherwise.
find({_, Key, _, Item, _} = _Tree, Key) ->
	Item;
find({Left, K, _, _, _}, Key) when Key < K ->
	find(Left, Key);
find({_, _, _, _, Right}, Key) ->
	find(Right, Key).

-spec insert(Tree1, Key, Item) -> Tree2
	when
		Tree1 :: treap(),
		Key :: term(),
		Item :: term(),
		Tree2 :: treap().
%% @equiv insert(Tree1, Key, erlang:system_time(), Item)
insert(Tree1, Key, Item) ->
	insert(Tree1, Key, erlang:system_time(), Item).

-spec insert(Tree1, Key, Time, Item) -> Tree2
	when
		Tree1 :: treap(),
		Key :: term(),
		Time :: pos_integer(),
		Item :: term(),
		Tree2 :: treap().
%% @doc Inserts `Key' with value `Item' into `Tree1'.
%% 	Returns a new `treap()' in `Tree2'.
insert({Left, Key, _, _, Right} = _Tree1, Key, Time, Item) ->
	{Left, Key, Time, Item, Right};
insert({Left, K, T, I, Right}, Key, Time, Item) when Key < K ->
	rebalance({insert(Left, Key, Time, Item), K, T, I, Right});
insert({Left, K, T, I, Right}, Key, Time, Item) ->
	rebalance({Left, K, T, I, insert(Right, Key, Time, Item)});
insert(undefined, Key, Time, Item) ->
	{undefined, Key, Time, Item, undefined}.

-spec remove(Tree1, Key) -> Tree2
	when
		Tree1 :: treap(),
		Key :: term(),
		Tree2 :: treap().
%% @doc Remove `Key' from `Tree1'.
%% 	Returns a new `treap()' in `Tree2'.
remove({_, Key, _, _, _} = Tree1, Key) ->
	percolate(Tree1);
remove({Left, K, T, I, Right}, Key) when Key < K ->
	{remove(Left, Key), K, T, I, Right};
remove({Left, K, T, I, Right}, Key) ->
	{Left, K, T, I, remove(Right, Key)}.
	
-spec expire(Tree1, Time) -> Tree2
	when
		Tree1 :: treap(),
		Time :: pos_integer(),
		Tree2 :: treap().
%% @doc Efficiently emove every item older than `Time'.
%% 	Returns a new `treap()' in `Tree2'.
expire({_, K, T, _, _} = Tree1, Time) when T < Time ->
	expire(remove(Tree1, K), Time);
expire(Tree, _Time) ->
	Tree.

%%%
%%% internal functions
%%% 

-spec rebalance(Tree1) -> Tree2
	when
		Tree1 :: treap(),
		Tree2 :: treap().
%% @doc Rebalance through order-preserving node rotations.
%% @private
rebalance({{S1, K, T, I, S2}, Key, Time, Item, S3} = _Tree1) when Time > T ->
	{S1, K, T, I, {S2, Key, Time, Item, S3}};
rebalance({S1, Key, Time, Item, {S2, K, T, I, S3}}) when Time > T ->
	{{S1, Key, Time, Item, S2}, K, T, I, S3};
rebalance(Tree) ->
	Tree.

-spec percolate(Tree1) -> Tree2
	when
		Tree1 :: treap(),
		Tree2 :: treap().
%% @doc Rebalancing node rotations during removal.
%% @private
percolate({undefined, _, _, _, undefined} = _Tree1) ->
	undefined;
percolate({undefined, K, T, I, {Lr, Kr, Tr, Ir, Rr}}) ->
	{Lr, Kr, Tr, Ir, percolate({undefined, K, T, I, Rr})};
percolate({{Ll, Kl, Tl, Il, Rl}, K, T, I, undefined}) ->
	{percolate({Ll, K, T, I, undefined}), Kl, Tl, Il, Rl};
percolate({{Ll, Kl, Tl, Il, Rl}, K, T, I,
		{_, _, Tr, _, _} = Right}) when Tl =< Tr ->
	{percolate({Ll, K, T, I, Rl}), Kl, Tl, Il, Right};
percolate({{_, _, _, _, _} = Left, K, T, I, {Lr, Kr, Tr, Ir, Rr}}) ->
	{Left, Kr, Tr, Ir, percolate({Lr, K, T, I, Rr})}.

