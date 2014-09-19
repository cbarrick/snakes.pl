% snake.pl -- A snake-in-the-box solver
% Copyright (C) 2014  Chris Barrick <cbarrick1@gmail.com>
%
% Permission to use, copy, modify, and/or distribute this software for any
% purpose with or without fee is hereby granted, provided that the above
% copyright notice and this permission notice appear in all copies.
%
% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION
% OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN
% CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.


% Snake in the box
% ==================================================
% The Snake in the Box problem concerns itself with hypercube graphs. The
% N-dimensional hypercube graph is the regular graph with 2^N nodes of
% degree N. We label each node with N bits and let each adjacent node differ
% by only one bit. Thus we only need N (the dimension) to uniquely identify
% the graph.
%
% A "snake" is a path in the hypercube, ending at 0, such that each node in
% the path is not adjacent to any other node in the path except those
% immediatly before and after. The length of a snake is often defined as the
% number of _edges_; however, for programming's sake, we define the length
% as the number of _nodes_ here.
%
% Our goal is to identify really long snakes.


% 1: Graph definitions
% --------------------------------------------------

%% node(+Dimension, ?A)
% True when A is a node in the hypercube of a given Dimension.

node(Dimension, A) :-
	Dimension > 1,
	Max is (2 ^ Dimension) - 1,
	between(0, Max, A).


%% edge(+Dimension, ?A, ?B)
% True when there is an edge from A to B in the hypercube of a given Dimension.

edge(Dimension, A, B) :-
	node(Dimension, A),
	Dimension > 1,
	D0 is Dimension - 1,
	between(0, D0, D),
	B is xor(A, 2 ^ D).


%% path(+Dimension, ?Path)
% True when Path is a valid path in the hypercube of a given Dimension.
% This predicate generates the shortest paths first.

path(Dimension, [A]) :- node(Dimension, A).

path(Dimension, [A,B|T]) :-
	path(Dimension, [B|T]),
	edge(Dimension, A, B).


%% available(+Dimension, +Snake, ?Node)
% True when Node is reachable from the head of a Snake in the hypercube of
% a given Dimension such that `[Node|Snake]` is also a valid snake.

available(Dimension, Snake, Node) :-
	Snake = [H|T],
	edge(Dimension, H, Node),
	Node \= 0,
	\+ (member(N, T), edge(Dimension, N, Node)).


%% snake(+Dimension, ?Snake)
% True for any valid Snake in the given Dimension.
% This predicate generates the shortest snakes first.
% Results from this predicate are memo'd under the predicate `snake_memo/2`.

:- dynamic(snake_memo/2).

snake(_, [0]).

snake(Dimension, Snake) :-
	snake_memo(Dimension, Snake).

snake(Dimension, Snake) :-
	Snake = [A,B|T],
	NumberOfNodes is 2 ^ Dimension,
	between(1, NumberOfNodes, Length),
	length(Snake, Length),

	snake(Dimension, [B|T]),
	edge(Dimension, B, A),
	available(Dimension, [B|T], A),

	\+ snake_memo(Dimension, Snake),
	assertz( snake_memo(Dimension, Snake) ).


% 2: Search algorithms
% --------------------------------------------------

% ### 2.0: The interface

%% long_snake(+Dimension, -Snake)
% Gets a long Snake in a given Dimension.
% This is the general interface that all search algorithms expose.
% This predicate may be aliased to any of the concrete implementations.

long_snake(Dimension, Snake) :-
	long_snake_naive(Dimension, Snake).


% ### 2.1: Iterative deepening

%% long_snake_naive(+Dimension, -Snake)
% Implements an iterative deepening search for a long snake.

long_snake_naive(Dimension, Snake) :-
	format('long_snake_naive: iterative deepening search\n', []),
	format('working in dimension ~w\n', [Dimension]),

	% Start looking for snakes of length 1, longer snakes upon backtracking
	NumberOfNodes is 2 ^ Dimension,
	between(1, NumberOfNodes, SnakeLength),

	% Try to find any snake of this length
	length(Snake, SnakeLength),
	once(snake(Dimension, Snake)),
	format('  found length ~w: ~w\n', [SnakeLength, Snake]),

	% There must not be any snakes longer than this snake
	NextLength is SnakeLength + 1,
	length(NextSnake, NextLength),
	\+ snake(Dimension, NextSnake),
	format('  did not find length ~w\n', [NextLength]),
	format('  longest snake: ~w\n', [Snake]),
	!.


% ### 2.2: Genetic algorithm

%% long_snake_ga(+Dimension, -Snake)
% Implements a genetic algorithm search to find a long snake.

% TODO
