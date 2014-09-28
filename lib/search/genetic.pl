:- module(genetic, [

	long_snake/6

]).

:- use_module('lib/graph').
:- use_module('lib/sort').


%% biased_select(+Bias, -X, +List, -Rest)
%

biased_select(A/B, X, List, Rest) :-
	length(List, L),
	Bias is A / (L / B),
	biased_select(Bias, X, List, Rest).

biased_select(Bias, X, List, Rest) :-
	select(X, List, Rest),
	maybe(Bias),
	!.

biased_select(_, X, List, Rest) :-
	random_select(X, List, Rest).


%% biased_mate_select(+N, +Bias, -Selection, +List, -Rest)
%

:- dynamic(ga_selection_state/3).
ga_selection_state(n/a, n/a, 0.1).

biased_mate_select(0, _, [], List, List) :- !.

biased_mate_select(N, auto, Selection, List, Rest) :-
	List = [Best|_],
	retract(ga_selection_state(OldBest, OldState, _)),
	( Best = OldBest ->
		State is OldState + 1,
		Bias is max(0, 0.1 - 0.005 * State)
	;
		State = 0,
		Bias is 0.1
	),
	asserta(ga_selection_state(Best, State, Bias)),
	!,
	biased_mate_select(N, Bias, Selection, List, Rest).

biased_mate_select(N, Bias, Selection, List, Rest) :-
	Selection = [H|T],
	biased_select(Bias, H, List, Rest_),
	N0 is N - 1,
	biased_mate_select(N0, Bias, T, Rest_, Rest).


%% mutate(+Dimension, +Probability, +Original, -Mutant)
% Given a mutation Probability and an Original transition list in the hypercube
% of the given Dimension, Mutant is a list derived from the Original by
% inserting and removing transitions at random locations. Mutant will be the
% same length as the original. Mutations may stack, i.e. there chance of
% mutating N times is Probability^N

mutate(Dimension, P, Original, Mutant) :-
	maybe(P),
	random_select(_, Original, Smaller), % randomly remove an element
	random(0, Dimension, New),
	random_select(New, PartialMutant, Smaller),
	!,
	mutate(Dimension, P, PartialMutant, Mutant).

mutate(_, _, Original, Original) :- !.


%% offspring(+Mother, +Father, ?Child)
% True when Child is an offspring of Mother and Father. That is, at some
% crossover point, every element before that point in Child is from Mother,
% and every element after that point if from Father.

offspring(Mother, Father, Child) :-
	length(Mother, L),
	length(Father, L),
	between(0, L, Crossover),
	offspring_(Mother, Father, Child, Crossover).

offspring_([], [], [], _) :- !.

offspring_([MH|MT], [FH|FT], [CH|CT], Crossover) :-
	( Crossover > 0 ->
		CH = MH
	;
		CH = FH
	),
	NextCrossover is Crossover - 1,
	offspring_(MT, FT, CT, NextCrossover).


%% breed_population(+Dimension, +Parents, +SurvivalRate, +MutationRate, -Population)
% Given a list of Parents (transition lists for the hypercube of the given
% Dimension), a SurvivalRate, and a MutationRate, Population is a new population
% bread from the Parents. The first parent always carries over into the new
% population.

breed_population(Dimension, Parents, SurvivalRate, MutationRate, Population) :-
	length(Parents, L),
	L >= 2,
	Parents = [First|_],
	findall(Child, (
		select(Father, Parents, Rest),
		select(Mother, Rest, _),
		offspring(Mother, Father, Child_),
		maybe(SurvivalRate),
		mutate(Dimension, MutationRate, Child_, Child)
	), Population_),
	sort([First|Population_], Population).


%% random_path(+Dimension, -Transitions)
% Generate a random transition list for a path in the hypercube of the given
% Dimension. The transition list will have a length of
% ceiling(0.4 * 2 ^ Dimension), which is long enough to represent the longest
% snake.

random_path(Dimension, Transitions) :-

	% We know that the longest snake is bounded above by ceil(0.4 * 2 ^ D)
	% so we need not generate any longer paths
	% http://en.wikipedia.org/wiki/Snake-in-the-box
	Length is ceiling(0.4 * (2 ^ Dimension)) + 1,

	length(Transitions, Length),
	random_path_(Dimension, Transitions).

random_path_(_, []) :- !.

random_path_(Dimension, Transitions) :-
	Transitions = [H|T],
	random(0, Dimension, H),
	random_path_(Dimension, T).


%% random_population(+Dimension, +SurvivalRate, -Population)
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

random_population(Dimension, SurvivalRate, Population) :-
	random_path(Dimension, A),
	random_path(Dimension, B),
	random_path(Dimension, C),
	random_path(Dimension, D),
	breed_population(Dimension, [A,B,C,D], SurvivalRate, 0.99, Population).


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
	findall(Node, available(Dimension, Snake, Node), Available),
	length(Snake, X),
	length(Available, Y),
	Fitness is (Dimension * X) + Y,
	asserta(fitness_memo(Dimension, Transitions, Fitness)).


%% cleanup, cleanup_auto
% Cleans up the memos left by the `fitness/3` and `snake/2` predicates.
% The "auto" version only cleans up every 10th time that it is called.

cleanup :-
	flag('cleanup_auto last_cleanup', _, 0),
	retractall(fitness_memo(_,_,_)),
	retractall(snake_memo(_,_)).

cleanup_auto :-
	flag('cleanup_auto last_cleanup', N, N+1),
	( N =:= 9 ->
		cleanup
	;
		true
	).


%% long_snake(+Dimension, +N, +SelectionBias, +SurvivalRate, +MutationRate, -Snake)
% Find a long snake using a genetic algorithm. If N is negative, runs for that
% many generations. If N = 1, run indefinitly.

long_snake(Dimension, N, SelectionBias, SurvivalRate, MutationRate, Snake) :-
	format('genetic algorithm search\n', []),
	format('dimension: ~w\n', [Dimension]),
	format('selection bias: ~w\n', [SelectionBias]),
	format('survival rate: ~w\n', [SurvivalRate]),
	format('mutation rate: ~w\n', [MutationRate]),
	random_population(Dimension, SurvivalRate, Population),
	long_snake_ga_(Dimension, Population, N, SelectionBias, SurvivalRate, MutationRate, Snake).

long_snake_ga_(Dimension, Population, 0, _, _, _, Best) :-
	statistics(real_time, _),
	mergesort(Population, descending(fitness(Dimension)), PopulationSort),
	PopulationSort = [BestTransitions|_],
	statistics(real_time, [_, TimeTaken]),

	transitions(BestPath, BestTransitions),
	prune(Dimension, BestPath, Best),
	fitness(Dimension, BestTransitions, BestFitness),
	ga_selection_state(_, _, LastSelectionBias),
	length(PopulationSort, PopSize),
	length(Best, BestLength),
	format('generation 0:\n', []),
	format('  time = ~ws\n', [TimeTaken]),
	format('  population size = ~w\n', [PopSize]),
	format('  selection bias = ~w\n', [LastSelectionBias]),
	format('  best snake = ~w\n', [Best]),
	format('  length (nodes) = ~w\n', [BestLength]),
	format('  fitness = ~w\n', [BestFitness]),
	format('DONE\n'),
	!.

long_snake_ga_(Dimension, Population, N, SelectionBias, SurvivalRate, MutationRate, Best) :-
	statistics(real_time, _),
	mergesort(Population, descending(fitness(Dimension)), SortedPopulation),
	SortedPopulation = [CurrentBest|_],
	statistics(real_time, [_, TimeTaken]),

	cleanup_auto,

	transitions(CurrentBestPath, CurrentBest),
	prune(Dimension, CurrentBestPath, CurrentBestSnake),
	fitness(Dimension, CurrentBest, BestFitness),
	ga_selection_state(_, _, LastSelectionBias),
	length(SortedPopulation, PopSize),
	length(CurrentBestSnake, CurrentBestLength),
	format('generation ~w:\n', [N]),
	format('  time = ~ws\n', [TimeTaken]),
	format('  population size = ~w\n', [PopSize]),
	format('  selection bias = ~w\n', [LastSelectionBias]),
	format('  best snake = ~w\n', [CurrentBestSnake]),
	format('  length (nodes) = ~w\n', [CurrentBestLength]),
	format('  fitness = ~w\n', [BestFitness]),

	biased_mate_select(3, SelectionBias, [A,B,C], SortedPopulation, _),
	random_path(Dimension, Random),
	breed_population(Dimension, [A,B,C,Random], SurvivalRate, MutationRate, NextPopulation),
	N1 is N + 1,
	!,
	long_snake_ga_(Dimension, [CurrentBest|NextPopulation], N1, SelectionBias, SurvivalRate, MutationRate, Best).