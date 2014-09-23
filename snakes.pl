#!/usr/bin/env swipl -g main

% snakes.pl -- A snake-in-the-box solver
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


% 0: Graph definitions and helper predicates
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


%% transition_list(?Path, ?Transitions)
% Converts a path between a node-list representation and a transition-list
% representation. A transition list is a list of numbers corresponding to
% which bit is flipped between nodes. A transition-list has a length of 1
% less than the length of the path.

transition_list([0], []) :- !.

transition_list(Path, Transitions) :-
	Path = [PA,PB|PT],
	Transitions = [TH|TT],
	transition_list([PB|PT], TT),
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


%% prune(+Dimension, ?Path, ?Snake)
% Shortens a path until it is a valid snake.

prune(Dimension, Path, Path) :- snake(Dimension, Path), !.

prune(Dimension, Path, Snake) :-
	Path = [_|T],
	prune(Dimension, T, Snake).


%% quicksort(+List, +Comparator, -Sorted)
% True when Sorted contains the elements of List sorted by the Comparator.
% Duplicates are removed. The Comparator is the name of a 3-place predicate
% which takes two elements as the first arguments and unifies the third with
% the element which should be placed before the other.
%
% Implemented as concurrent quicksort; mergesort appears to be faster.

quicksort([], _, []) :- !.

quicksort(List, Comparator, Sorted) :-
	sort(List, CleanList), % remove duplicates
	CleanList = [Pivot|Unsorted],
	quicksort_([Pivot|Unsorted], [], [], Comparator, Sorted).

quicksort_([Pivot], Before, After, Comparator, Sorted) :-
	thread_create(
		(
			quicksort(Before, Comparator, SortedBefore),
			thread_exit(SortedBefore)
		),
		BeforeThread,
		[]
	),
	thread_create(
		(
			quicksort(After, Comparator, SortedAfter),
			thread_exit(SortedAfter)
		),
		AfterThread,
		[]
	),
	thread_join(BeforeThread, exited(SortedBefore)),
	thread_join(AfterThread, exited(SortedAfter)),
	append([SortedBefore, [Pivot], SortedAfter], Sorted),
	!.

quicksort_([Pivot,Next|Unsorted], Before, After, Comparator, Sorted) :-
	call(Comparator, Pivot, Next, Next),
	!,
	quicksort_([Pivot|Unsorted], [Next|Before], After, Comparator, Sorted).

quicksort_([Pivot,Next|Unsorted], Before, After, Comparator, Sorted) :-
	call(Comparator, Pivot, Next, Pivot),
	!,
	quicksort_([Pivot|Unsorted], Before, [Next|After], Comparator, Sorted).


%% mergesort(+List, +Comparator, -Sorted)
% True when Sorted contains the elements of List sorted by the Comparator.
% Duplicates are removed. The Comparator is the name of a 3-place predicate
% which takes two elements as the first arguments and unifies the third with
% the element which should be placed before the other.
%
% Implemented as concurrent mergesort; this appears to be faster than quicksort.

mergesort([], _, []) :- !.
mergesort([X], _, [X]) :- !.

mergesort(List, Comp, Sorted) :-
	sort(List, CleanList), % remove duplicates.
	length(CleanList, L),
	Half is L // 2,
	length(Front, Half),
	append(Front, Back, CleanList),

	thread_create(
		(
			mergesort(Front, Comp, SortedFront),
			thread_exit(SortedFront)
		),
		FrontThread,
		[]
	),
	thread_create(
		(
			mergesort(Back, Comp, SortedBack),
			thread_exit(SortedBack)
		),
		BackThread,
		[]
	),
	thread_join(FrontThread, exited(SortedFront)),
	thread_join(BackThread, exited(SortedBack)),

	merge(SortedFront, SortedBack, Comp, Sorted).

merge([], Ys, _, Ys) :- !.
merge(Xs, [], _, Xs) :- !.

merge([X|XT], [Y|YT], Comp, Merged) :-
	call(Comp, X, Y, X),
	!,
	Merged = [X|Rest],
	merge(XT, [Y|YT], Comp, Rest).

merge([X|XT], [Y|YT], Comp, Merged) :-
	call(Comp, X, Y, Y),
	!,
	Merged = [Y|Rest],
	merge([X|XT], YT, Comp, Rest).


%% ascending(+A, +B, -C), ascending(+Evaluator, +A, +B, -C)
%% descending(+A, +B, -C), descending(+Evaluator, +A, +B, -C)
% Given an Evaluator predicate, bind C to either A or B. The Evaluator is
% called with two additional arguments; the first is the item being evaluated,
% and the second is a variable which should become bound to a numeric value
% for the item. `ascending` binds C to the item with the smallest value, and
% `descending` binds C to the item with the largest value. If no evaluator is
% given, the arguments are compared directly.

ascending(A, B, C) :-
	A < B,
	C = A.

ascending(A, B, C) :-
	A >= B,
	C = B.

ascending(Eval, A, B, C) :-
	call(Eval, A, ValueA),
	call(Eval, B, ValueB),
	( ValueA < ValueB ->
		C = A
	;
		C = B
	),
	!.

descending(A, B, C) :-
	A > B,
	C = A.

descending(A, B, C) :-
	A =< B,
	C = B.

descending(Eval, A, B, C) :-
	call(Eval, A, ValueA),
	call(Eval, B, ValueB),
	( ValueA > ValueB ->
		C = A
	;
		C = B
	),
	!.


% 1: Iterative deepening
% --------------------------------------------------

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
	format('found length ~w: ~w\n', [SnakeLength, Snake]),

	% There must not be any snakes longer than this snake
	NextLength is SnakeLength + 1,
	length(NextSnake, NextLength),
	\+ snake(Dimension, NextSnake),
	format('did not find length ~w\n', [NextLength]),
	format('longest snake: ~w\n', [Snake]),
	!.


% 2: Genetic algorithm
% --------------------------------------------------

%% ga_mutate(+Dimension, +Probability, +Original, -Mutant)
% Given a mutation Probability and an Original transition list in the hypercube
% of the given Dimension, Mutant is a list derived from the Original by
% inserting and removing transitions at random locations. Mutant will be the
% same length as the original. Mutations may stack, i.e. there chance of
% mutating N times is Probability^N

ga_mutate(Dimension, P, Original, Mutant) :-
	maybe(P),
	random_select(_, Original, Smaller), % randomly remove an element
	random(0, Dimension, New),
	random_select(New, PartialMutant, Smaller),
	!,
	ga_mutate(Dimension, P, PartialMutant, Mutant).

ga_mutate(_, _, Original, Original) :- !.


%% ga_offspring(+Mother, +Father, ?Child)
% True when Child is an offspring of Mother and Father. That is, at some
% crossover point, every element before that point in Child is from Mother,
% and every element after that point if from Father.

ga_offspring(Mother, Father, Child) :-
	length(Mother, L),
	length(Father, L),
	between(0, L, Crossover),
	ga_offspring_(Mother, Father, Child, Crossover).

ga_offspring_([], [], [], _) :- !.

ga_offspring_([MH|MT], [FH|FT], [CH|CT], Crossover) :-
	( Crossover > 0 ->
		CH = MH
	;
		CH = FH
	),
	NextCrossover is Crossover - 1,
	ga_offspring_(MT, FT, CT, NextCrossover).


%% ga_breed_population(+Dimension, +Parents, +SurvivalRate, +MutationRate, -Population)
% Given a list of Parents (transition lists for the hypercube of the given
% Dimension), a SurvivalRate, and a MutationRate, Population is a new population
% bread from the Parents. The first parent always carries over into the new
% population.

ga_breed_population(Dimension, Parents, SurvivalRate, MutationRate, Population) :-
	length(Parents, L),
	L >= 2,
	Parents = [First|_],
	findall(Child, (
		select(Father, Parents, Rest),
		select(Mother, Rest, _),
		ga_offspring(Mother, Father, Child_),
		maybe(SurvivalRate),
		ga_mutate(Dimension, MutationRate, Child_, Child)
	), Population_),
	sort([First|Population_], Population).


%% ga_random_path(+Dimension, -Transitions)
% Generate a random transition list for a path in the hypercube of the given
% Dimension. The transition list will have a length of
% ceiling(0.4 * 2 ^ Dimension), which is long enough to represent the longest
% snake.

ga_random_path(Dimension, Transitions) :-

	% We know that the longest snake is bounded above by ceil(0.4 * 2 ^ D)
	% so we need not generate any longer paths
	% http://en.wikipedia.org/wiki/Snake-in-the-box
	Length is ceiling(0.4 * (2 ^ Dimension)) + 1,

	length(Transitions, Length),
	ga_random_path_(Dimension, Transitions).

ga_random_path_(_, []) :- !.

ga_random_path_(Dimension, Transitions) :-
	Transitions = [H|T],
	random(0, Dimension, H),
	ga_random_path_(Dimension, T).


%% ga_random_population(+Dimension, +SurvivalRate, -Population)
% Generate a Population of random transition lists for paths in the hypercube
% of the given Dimension. Each transition list will have a length of
% ceiling(0.4 * 2 ^ Dimension), which is long enough to represent the longest
% snake.
%
% The implementation works by breeding 4 random paths with each other using
% a high mutation rate and the given SurvivalRate. This is slower than simply
% generating a whole bunch of random paths, but has the benifit that the size
% of the random population will be of the same scale as non-random populations
% bred with the same SurvivalRate and number of parents.

ga_random_population(Dimension, SurvivalRate, Population) :-
	ga_random_path(Dimension, A),
	ga_random_path(Dimension, B),
	ga_random_path(Dimension, C),
	ga_random_path(Dimension, D),
	ga_breed_population(Dimension, [A,B,C,D], SurvivalRate, 0.99, Population).


%% ga_fitness(+Dimension, +Transitions, -Fitness)
% Calculate the Fitness score for a list of Transitions describing a path
% in the hypercube of the given Dimension.

ga_fitness(Dimension, Transitions, Fitness) :-
	transition_list(Path, Transitions),
	prune(Dimension, Path, Snake),
	findall(Node, available(Dimension, Snake, Node), Available),
	length(Snake, X),
	length(Available, Y),
	Fitness is (Dimension * X) + Y.


%% long_snake_ga(+Dimension, +N, +SurvivalRate, +MutationRate, -Snake)
% Find a long snake using a genetic algorithm. If N is negative, runs for that
% many generations. If N = 1, run indefinitly.

long_snake_ga(Dimension, N, SurvivalRate, MutationRate, Snake) :-
	format('long_snake_ga: genetic algorithm search\n', []),
	format('dimension: ~w\n', [Dimension]),
	format('survival rate: ~w\n', [SurvivalRate]),
	format('mutation rate: ~w\n', [MutationRate]),
	ga_random_population(Dimension, SurvivalRate, Population),
	long_snake_ga_(Dimension, Population, N, SurvivalRate, MutationRate, Snake).

long_snake_ga_(Dimension, Population, 0, _, _, Best) :-
	mergesort(Population, descending(ga_fitness(Dimension)), PopulationSort),
	PopulationSort = [BestTransitions|_],
	transition_list(BestPath, BestTransitions),
	prune(Dimension, BestPath, Best),

	ga_fitness(Dimension, BestTransitions, BestFitness),
	length(PopulationSort, PopSize),
	length(Best, BestLength),
	format('generation 0:\n', []),
	format('  population size = ~w\n', [PopSize]),
	format('  best snake = ~w\n', [Best]),
	format('  length (nodes) = ~w\n', [BestLength]),
	format('  fitness = ~w\n', [BestFitness]),
	format('DONE\n'),
	!.

long_snake_ga_(Dimension, Population, N, SurvivalRate, MutationRate, Best) :-
	mergesort(Population, descending(ga_fitness(Dimension)), SortedPopulation),
	SortedPopulation = [A,B|_],

	transition_list(CurrentBestPath, A),
	prune(Dimension, CurrentBestPath, CurrentBestSnake),
	ga_fitness(Dimension, A, BestFitness),
	length(SortedPopulation, PopSize),
	length(CurrentBestSnake, CurrentBestLength),
	format('generation ~w:\n', [N]),
	format('  population size = ~w\n', [PopSize]),
	format('  best snake = ~w\n', [CurrentBestSnake]),
	format('  length (nodes) = ~w\n', [CurrentBestLength]),
	format('  fitness = ~w\n', [BestFitness]),

	ga_random_path(Dimension, Random1),
	ga_random_path(Dimension, Random2),
	ga_breed_population(Dimension, [A,B,Random1,Random2], SurvivalRate, MutationRate, NextPopulation),
	N1 is N + 1,
	!,
	long_snake_ga_(Dimension, [A,B|NextPopulation], N1, SurvivalRate, MutationRate, Best).




% Experiments
% --------------------------------------------------

% An experiment on sorting algorithms
% Mergesort seems to be fastest
sorting_test :-
	D = 3,
	N = 500,
	ga_random_population(D, N, Pop),
	descending(ga_fitness(D), Comp),

	format('Sorting ~w random paths in dimension ~w\n', [N, D]),

	format('\nquicksort:\n'),
	time(quicksort(Pop, Comp, Sorted1)),

	format('\nmergesort\n'),
	time(mergesort(Pop, Comp, Sorted2)),

	% Check that the lists are equivalent
	% The sorted lists may not be in the same order given that some paths
	% have the same fitness. So we must check against the fitness of the
	% paths in the lists.
	findall(X, (
		member(A, Sorted1),
		ga_fitness(D, A, X)
	), Xs),
	findall(Y, (
		member(A, Sorted2),
		ga_fitness(D, A, Y)
	), Ys),
	format('~w\n', Xs),
	format('~w\n', Ys),
	Xs = Ys.


% Find the longest snake in 7D using the GA
% This will run indefinitly, printing results after each generation
main :-

	% The longest snake in 7D is 51 nodes
	Dimension = 7,

	% When breading, not all possible children are returned. Some children "die"
	% before becoming productive. This determines how many children survive.
	% Lower rates mean each generation is faster, higher rates mean each
	% generation is better. The goal is to optimize execution speed.
	SurvivalRate = 0.25,

	% Mutations stack, i.e. at 0.1 there is a 10% chance to mutate once,
	% a 1% chance to mutate twice, 0.1% chance to mutate 3 times, etc
	MutationRate = 0.1,

	long_snake_ga(Dimension, 1, SurvivalRate, MutationRate, _).
