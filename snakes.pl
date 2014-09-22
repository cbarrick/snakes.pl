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


%% long_snake(+Dimension, -Snake)
% Gets a long Snake in a given Dimension.
% This is the general interface that all search algorithms expose.
% This predicate may be aliased to any of the concrete implementations.

long_snake(Dimension, Snake) :-
	long_snake_naive(Dimension, Snake).


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

	% mergesort(Front, Comp, SortedFront),
	% mergesort(Back, Comp, SortedBack),

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


%% comparator(+Evaluator, -Comparator)
%% comparator_inverse(+Evaluator, -Comparator)
%

% Both predicates memo their results to avoid asserting too many rules
:- dynamic(comparator_memo/2).
:- dynamic(comparator_inverse_memo/2).

% Creates a comparator that orders items from smallest to biggest
comparator(Eval, Comp) :-
	comparator_memo(Eval, Comp),
	!.

comparator(Eval, Comp) :-
	gensym(comparator, Comp),
	Head =.. [Comp, A, B, C],
	Rule = (
		Head :-
			call(Eval, A, ValueA),
			call(Eval, B, ValueB),
			( ValueA < ValueB ->
				C = A
			;
				C = B
			),
			!
	),
	asserta(Rule),
	asserta(comparator_memo(Eval, Comp)),
	!.

% Creates a comparator that orders item from biggest to smallest
comparator_inverse(Eval, Comp) :-
	comparator_inverse_memo(Eval, Comp),
	!.

comparator_inverse(Eval, Comp) :-
	gensym(comparator, Comp),
	Head =.. [Comp, A, B, C],
	Rule = (
		Head :-
			call(Eval, A, ValueA),
			call(Eval, B, ValueB),
			( ValueA > ValueB ->
				C = A
			;
				C = B
			),
			!
	),
	asserta(Rule),
	asserta(comparator_inverse_memo(Eval, Comp)),
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
	format('  found length ~w: ~w\n', [SnakeLength, Snake]),

	% There must not be any snakes longer than this snake
	NextLength is SnakeLength + 1,
	length(NextSnake, NextLength),
	\+ snake(Dimension, NextSnake),
	format('  did not find length ~w\n', [NextLength]),
	format('  longest snake: ~w\n', [Snake]),
	!.


% 2: Genetic algorithm
% --------------------------------------------------

%% ga_offspring(+Mother, +Father, ?Child)
%

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


%% ga_breed_population(...)
%

ga_breed_population(Parents, Population) :-
	length(Parents, L),
	L >= 2,
	findall(Child, (
		select(Father, Parents, Rest),
		select(Mother, Rest, _),
		ga_offspring(Mother, Father, Child)
	), Population_),
	sort(Population_, Population).


%% ga_random_path(+Dimension, -Transitions)
%

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


%% ga_random_population(...)
%

ga_random_population(_, 0, []) :- !.

ga_random_population(Dimension, Size, Population) :-
	Population = [H|T],
	ga_random_path(Dimension, H),
	S0 is Size - 1,
	ga_random_population(Dimension, S0, T).


%% ga_fitness(...)
%

ga_fitness(Dimension, Transitions, Fitness) :-
	transition_list(Path, Transitions),
	prune(Dimension, Path, Snake),
	findall(Node, available(Dimension, Snake, Node), Available),
	length(Snake, X),
	length(Available, Y),
	Fitness is (Dimension * X) + Y.


%% long_snake_ga(...)
%

long_snake_ga(Dimension, Snake) :-
	long_snake_ga(Dimension, -500, Snake).

long_snake_ga(Dimension, N, Snake) :-
	format('long_snake_ga: genetic algorithm search\n', []),
	format('working in dimension ~w\n', [Dimension]),
	ga_random_population(Dimension, 100, Population),
	long_snake_ga_(Dimension, Population, N, Snake).

long_snake_ga_(Dimension, Population, 0, Best) :-
	comparator_inverse(ga_fitness(Dimension), Comp),
	mergesort(Population, Comp, SortedPop),
	SortedPop = [BestTransitions|_],
	transition_list(BestPath, BestTransitions),
	prune(Dimension, BestPath, Best),

	ga_fitness(Dimension, BestTransitions, BestFitness),
	length(Population, PopSize),
	length(Best, BestLength),
	format('  generation 0:\n', []),
	format('    population size = ~w\n', [PopSize]),
	format('    best snake = ~w\n', [Best]),
	format('    length (nodes) = ~w\n', [BestLength]),
	format('    fitness = ~w\n', [BestFitness]),
	!.

long_snake_ga_(Dimension, Population, Generation, Best) :-
	comparator_inverse(ga_fitness(Dimension), Comp),
	mergesort(Population, Comp, SortedPop),
	SortedPop = [A,B|_],

	transition_list(CurrentBestPath, A),
	prune(Dimension, CurrentBestPath, CurrentBestSnake),
	ga_fitness(Dimension, A, BestFitness),
	length(Population, PopSize),
	length(CurrentBestSnake, CurrentBestLength),
	format('  generation ~w:\n', [Generation]),
	format('    population size = ~w\n', [PopSize]),
	format('    best snake = ~w\n', [CurrentBestSnake]),
	format('    length (nodes) = ~w\n', [CurrentBestLength]),
	format('    fitness = ~w\n', [BestFitness]),

	ga_random_path(Dimension, Random1),
	ga_random_path(Dimension, Random2),
	ga_breed_population([A,B,Random1,Random2], NextPopulation),
	NextGeneration is Generation + 1,
	!,
	long_snake_ga_(Dimension, [A,B|NextPopulation], NextGeneration, Best).




% Experiments
% --------------------------------------------------

% An experiment on sorting algorithms
sorting_test :-
	D = 3,
	N = 500,
	ga_random_population(D, N, Pop),
	comparator_inverse(ga_fitness(D), Comp),

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
main :-
	long_snake_ga(7, 1, _).
