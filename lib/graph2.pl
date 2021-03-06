:- module(graph, [

	node/2,
	edge/3,
	path/2,
	available/3,
	reachable/3,
	snake/2,
	snake/3,
	skin_density/4,
	fitness/3,
	random_population/3,
	pad/3,
	grow_population/3,
	grow_initial_population/4,
	xor_mutant/2,
	cleanup/0,
	cleanup_auto/1

]).


%% node(+D, ?A) [nondet]
% True when A is a node in the D-hypercube.

node(D, A) :-
	D > 1,
	Max is (2 ^ D) - 1,
	between(0, Max, A).


%% edge(+D, ?A, ?B) [nondet]
% True when there is an edge from A to B in the D-hypercube.

edge(D, A, B) :-
	node(D, A),
	D > 1,
	D0 is D - 1,
	between(0, D0, P),
	B is xor(A, 2 ^ P).


%% path(+D, ?NodeList) [nondet]
% True when NodeList describes the same path in the D-hypercube.

path(_, [0]).

path(D, [A,B|NodeList]) :-
	edge(D, A, B),
	path(D, [B|NodeList]).

%% available(+D, +Path, ?N) [nondet]
% True when N is an available node given the Path. N may not be reachable.

available(D, [_|Path], N) :-
	node(D, N),
	forall(member(X, Path), (
		N \= X,
		\+ edge(D, X, N)
	)).

%% reachable(+D, +Snake, ?N) [nondet]
% True when N is reachable from the head of the Snake.

reachable(D, [H|Snake], N) :-
	edge(D, H, N),
	once(available(D, [H|Snake], N)).


%% snake(+D, +Path) [nondet]
% True when Path is a snake

:- dynamic snake_memo/2.

snake(_, [0]).

snake(D, Path) :-
	snake_memo(Path, D).

snake(D, [A,B|Path]) :-
	MaxLength is ceiling(0.4 * 3^D),
	length([A,B|Path], Length),
	between(1, MaxLength, Length),
	path(D, [A,B|Path]),
	reachable(D, [B|Path], A),
	snake(D, [B|Path]),
	\+ snake_memo([A,B|Path], D),
	assertz( snake_memo([A,B|Path], D) ).


%% snake(+D, +Path, -Snake) [det]
% Shortens Path unitl it is a valid Snake

:- dynamic snake_memo/3.

snake(D, Path, Snake) :-
	snake_memo(Path, Snake, D),
	!.

snake(D, Path, Path) :-
	snake(D, Path),
	!.

snake(D, [H|Path], Snake) :-
	snake(D, Path, Snake),
	asserta( snake_memo([H|Path], Snake, D) ).


%% skin_density(+D, +Path, +Node, -Density) [det]
% Calculate the skin density of a Node given the Path

skin_density(D, Path, Node, Density) :-
	\+ member(Node, Path),
	findall(X, (
		edge(D, X, Node),
		member(X, Path)
	), Xs),
	length(Xs, Density),
	!.

skin_density(_, _, _, 0).


%% fitness(+D, +Path, -Fitness) [det]
% Calculates the fitness of a path

:- dynamic fitness_memo/3.

fitness(D, Path, Fitness) :-
	fitness_memo(Path, Fitness, D),
	debug(fitness, 'fitness cache hit', []),
	!.

fitness(D, Path, Fitness) :-
	snake(D, Path, Snake),
	length(Snake, Length),
	findall(Density, (
		available(D, Snake, Node),
		skin_density(D, Snake, Node, Density)
	), Densities),
	sum_list(Densities, Tightness),
	Fitness is Length * Length + Tightness,
	asserta( fitness_memo(Path, Fitness, D) ),
	!.


%% random_population(+D, +Size, -Population)
% DEPRECATED: Use grow_initial_population/4

random_population(_, 0, []) :- !.

random_population(D, Size, Population) :-
	grow_initial_population(D, Size, [0], Population).


%% pad(+D, +Seed, -Path)
%

pad(D, [H|Seed], Path) :-
	length(Seed, L),
	MaxLength is ceiling(0.4 * 2 ^ D),
	L < MaxLength,
	findall(N, (
		edge(D, H, N),
		\+ member(N, Seed)
	), NextNodes),
	random_select(Next, NextNodes, _),
	!,
	pad(D, [Next,H|Seed], Path).

pad(_, Seed, Seed).


%% grow_population(+D, +Population, -Children)
%

grow_population(_, [], []) :- !.

grow_population(D, [X|Population], [Child|Children]) :-
	snake(D, X, [H|T]),
	findall(Node, (
		edge(D, Node, H),
		snake(D, [Node,H|T])
	), Nodes),
	(
		random_select(Node, Nodes, _),
		pad(D, [Node,H|T], Child),
		grow_population(D, Population, Children)
	;
		grow_population(D, Population, [Child|Children])
	).


%% grow_initial_population(+D, +Size, +Seed, -Population)
%

grow_initial_population(D, Size, Seed, Population) :-
	length(Population, Size),
	findall(X, (
		member(X, Population),
		pad(D, Seed, X)
	), Population).

%% xor_mutant(+Original, -Mutant) [nondet]
%

xor_mutant(Original, Mutant) :-
	append([Front, [A,B,C], Back], Original),
	X is A xor C,
	Y is B xor X,
	append([Front, [A,Y,C], Back], Mutant).


%% cleanup / cleanup_auto(+Freq)
%

cleanup :-
	retractall(fitness_memo(_,_,_)),
	retractall(snake_memo(_,_,_)),
	retractall(snake_memo(_,_)),
	garbage_collect.

cleanup_auto(Freq) :-
	flag(last_cleanup, N, N+1),
	(N >= Freq ->
		cleanup
	;
		true
	).
