:- module(graph, [

	node/2,         % node(+Dimension, ?Node)
	edge/3,         % edge(+Dimension, ?A, ?B)
	path/2,         % path(+Dimension, ?Path)
	transitions/2,  % transitions(?Path, ?Transitions)
	available/3,    % available(+Dimension, +Snake, ?Node)
	reachable/3,    % reachable(+Dimension, +Snake, ?Node)
	snake/2,        % snake(+Dimension, ?Snake)
	skin_density/4, % skin_density(+Dimension, +Path, +Node, -Density)
	prune/3         % prune(+Dimension, +Path, -Snake)

]).


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


%% transitions(?Path, ?Transitions)
% Converts a path between a node-list representation and a transition-list
% representation. A transition list is a list of numbers corresponding to
% which bit is flipped between nodes. A transition-list has a length of 1
% less than the length of the path.

transitions([0], []) :- !.

transitions(Path, Transitions) :-
	Path = [PA,PB|PT],
	Transitions = [TH|TT],
	transitions([PB|PT], TT),
	( var(TH) ->
		TH is integer( log(xor(PA, PB))/log(2) )
	;
		PA is xor(PB, 2^TH)
	).


%% available(+Dimension, +Snake, ?Node)
% True when a Node in the hypercube of a given Dimension has not been excluded
% by the Snake. The Node is not necessicarily reachable by the snake.

available(Dimension, Snake, Node) :-
	Snake = [H|T],
	node(Dimension, Node),
	Node \= H,
	available_(Dimension, T, Node).

available_(Dimension, [], Node) :-
	node(Dimension, Node).

available_(Dimension, [H|T], Node) :-
	node(Dimension, Node),
	Node \= H,
	\+ edge(Dimension, H, Node),
	available_(Dimension, T, Node).


%% reachable(+Dimension, +Snake, ?Node)
% True when Node is reachable from the head of a Snake in the hypercube of
% a given Dimension such that `[Node|Snake]` is also a valid snake.

reachable(Dimension, Snake, Node) :-
	Snake = [H|_],
	edge(Dimension, H, Node),
	available(Dimension, Snake, Node).


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
	reachable(Dimension, [B|T], A),

	\+ snake_memo(Dimension, Snake),
	assertz( snake_memo(Dimension, Snake) ).


%% skin_density(+Dimension, +Path, +Node, -Density)
% Calculate the skin density of a Node with a given Path. The skin density is
% the number of nodes in the path to which the Node is adjacent. Nodes on the
% path have a skin density of 0.

skin_density(Dimension, Path, Node, Density) :-
	\+ member(Node, Path),
	findall(X, (edge(Dimension, Node, X), member(X, Path)), Xs),
	length(Xs, Density),
	!.

skin_density(_, _, _, 0).


%% prune(+Dimension, ?Path, ?Snake)
% Shortens a path until it is a valid snake.

prune(Dimension, Path, Path) :- snake(Dimension, Path), !.

prune(Dimension, Path, Snake) :-
	Path = [_|T],
	prune(Dimension, T, Snake).


%% cleanup
% Cleans memos left by this module. This predicate is not explicitly exported.
% To call it, you must provide the module, i.e. `?- graph:cleanup.`

cleanup :-
	retractall(graph:snake_memo(_,_)).
