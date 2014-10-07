:- module(graph, [

	node/2,         % node(+Dimension, ?Node)
	edge/3,         % edge(+Dimension, ?A, ?B)
	path/2,         % path(+Dimension, ?Path)
	transitions/2,  % transitions(?Path, ?Transitions)
	available/3,    % available(+Dimension, +Snake, ?Node)
	reachable/3,    % reachable(+Dimension, +Snake, ?Node)
	snake/2,        % snake(+Dimension, ?Snake)
	skin_density/4, % skin_density(+Dimension, +Path, +Node, -Density)
	prune/3,        % prune(+Dimension, +Path, -Snake)
	fitness/3,      % fitness(+Dimension, +Transitions, -Fitness)
	random_paths/3, % random_paths(+Transitions, +N, -Paths)
	mutant/3,       % mutant(+Dimension, ?Transitions, ?Mutant)
	cleanup/0,      % cleanup
	cleanup_auto/1  % cleanup_auto(+Freq)

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


%% fitness(+Dimension, +Transitions, -Fitness)
% Calculate the Fitness score for a list of Transitions describing a path
% in the hypercube of the given Dimension.

:- dynamic(fitness_memo/3).

fitness(Dimension, Transitions, Fitness) :-
	fitness_memo(Dimension, Transitions, Fitness),
	!.

fitness(Dimension, Transitions, Fitness) :-
	transitions(Path, Transitions),
	prune(Dimension, Path, Snake),
	length(Snake, Length),
	findall(D, (node(Dimension, Node), skin_density(Dimension, Snake, Node, D)), Densities),
	sum_list(Densities, Density),
	Fitness is Length * Length + Density,
	asserta(fitness_memo(Dimension, Transitions, Fitness)).


%% random_paths(+Dimension, +N, -ListOfTransitions)
% Generate a list of N random paths as transition-lists for the hypercube of
% the given Dimension.

random_paths(Dimension, N, ListOfTransitions) :-
	length(ListOfTransitions, N),
	Length is ceiling(0.4 * (2 ^ Dimension)),
	findall(X, (
		member(X, ListOfTransitions),
		length(X, Length)
	), ListOfTransitions),
	random_paths_(Dimension, N, ListOfTransitions).

random_paths_(_, 0, []) :- !.

random_paths_(Dimension, N, [[]|ListOfTransitions]) :-
	N0 is N-1,
	!,
	random_paths_(Dimension, N0, ListOfTransitions).

random_paths_(Dimension, N, [[H|T]|ListOfTransitions]) :-
	D0 is Dimension - 1,
	random_between(0, D0, H),
	!,
	random_paths_(Dimension, N, [T|ListOfTransitions]).


%% mutant(+Dimension, ?Transitions, ?Mutant)
% True when Mutant is a single-point mutation of Transitions

mutant(Dimension, Transitions, Mutant) :-
	length(Transitions, L),
	between(0, L, MutationPoint),
	length(Front, MutationPoint),
	append([Front, [X], Back], Transitions),
	D0 is Dimension-1,
	between(0, D0, Y),
	X \= Y,
	append([Front, [Y], Back], Mutant).


%% cleanup / cleanup_auto(Freq)
% Cleans memos left by this module. This predicate is not explicitly exported.
% To call it, you must provide the module, i.e. `?- graph:cleanup.`

cleanup :-
	retractall(graph:snake_memo(_,_)),
	retractall(graph:fitness_memo(_,_,_)).

cleanup_auto(Freq) :-
	flag(graph:last_cleanup, N, N+1),
	(N >= Freq ->
		cleanup,
		flag(graph:last_cleanup, _, 0)
	;
		true
	).
