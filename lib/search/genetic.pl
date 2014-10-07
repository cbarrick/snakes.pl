:- module(genetic, [

	genetic_search/7,
	rank_select/4,
	random_npoint_crossover/4,
	identity/2,
	hillclimber/4,
	population_hillclimber/4

]).

:- use_module('lib/sort').


% General Genetic Search
% --------------------------------------------------

%% genetic_search(+Population, +Select, +Reproduce, +Mutate, +Terminate, -Soln)
% TODO: Document

:- meta_predicate genetic_search(+, 2, 3, 2, 3, 3, -).

genetic_search(Population, Fitness, Select, Reproduce, Mutate, Terminate, Soln) :-
	genetic_search_(0, Population, Fitness, Select, Reproduce, Mutate, Terminate, Soln).

genetic_search_(N, Population, _, _, _, _, Terminate, Soln) :-
	call(Terminate, N, Population, Soln),
	!.

genetic_search_(N, Population, Fitness, Select, Reproduce, Mutate, Terminate, Soln) :-

	% Selection
	debug(genetic_search, 'starting selection', []),
	once(call(Select, Fitness, Population, Parents)),

	% Reproduction
	debug(genetic_search, 'starting reproduction', []),
	once(call(Reproduce, Parents, Children)),

	% Mutation
	debug(genetic_search, 'starting mutation', []),
	once(call(Mutate, Fitness, Children, Mutants)),

	% Elitism
	% NOTE: calling mergesort hear is only OK if the fitnesses are known
	length(Population, L0),
	length(Mutants, L1),
	Delta is L0 - L1,
	length(Elite, Delta),
	mergesort(Population, descending(Fitness), Sorted),
	append(Elite, _, Sorted),
	append(Elite, Mutants, NextGeneration),

	N1 is N+1,
	!,
	genetic_search_(N1, NextGeneration, Fitness, Select, Reproduce, Mutate, Terminate, Soln).


% Helpers
% --------------------------------------------------

%% biased_select(+Bias, -X, +List, -Rest)
% Randomly select an element X from the List with a Bias for elements closer
% to the head of the List. Bias is a value between 0.0 and 1.0.

biased_select(Bias, X, List, Rest) :-
	select(X, List, Rest),
	maybe(Bias),
	!.

biased_select(_, X, List, Rest) :-
	random_select(X, List, Rest).


%% identity(?X, ?X)
% Use this for no mutation

identity(X, X).


% Selection
% --------------------------------------------------

%% rank_select(N, Fitness, Population, Parents)
% TODO: Document

:- meta_predicate rank_select(+, 2, +, -).

rank_select(N, Fitness, Population, Parents) :-
	sort(Population, Population_NoDupes), % remove duplicates
	length(Population_NoDupes, PopSize),
	(PopSize =< N ->
		Parents = Population
	;
		mergesort(Population_NoDupes, descending(Fitness), SortedPopulation),
		rank_select_(N, SortedPopulation, Parents)
	).

rank_select_(0, _, []) :- !.

rank_select_(N, Population, [H|Parents]) :-
	length(Population, L),
	Bias is L/((1/2)*(L)*(L+1)),
	biased_select(Bias, H, Population, Rest),
	N0 is N-1,
	rank_select_(N0, Rest, Parents).


% Crossover
% --------------------------------------------------

%% random_npoint_crossover(+Size, +N, +Parents, -Children)
% TODO: Document

random_npoint_crossover(0, _, _, []) :- !.

random_npoint_crossover(Size, N, Parents, [Child|Children]) :-
	random_select(Mother, Parents, Rest),
	random_select(Father, Rest, _),
	random_npoint_crossover_(N, Mother, Father, Child),
	Size0 is Size-1,
	!,
	random_npoint_crossover(Size0, N, Parents, Children).

random_npoint_crossover_(0, Mother, _, Mother) :- !.

random_npoint_crossover_(N, Mother, Father, Child) :-
	length(Mother, L),
	length(Child, L),
	Max is L - N + 1,
	random_between(0, Max, CrossPoint),
	length(MomFront, CrossPoint),
	length(DadFront, CrossPoint),
	append(MomFront, MomBack, Mother),
	append(DadFront, DadBack, Father),
	append(MomFront, ChildBack, Child),
	N0 is N-1,
	!,
	random_npoint_crossover_(N0, DadBack, MomBack, ChildBack).


% Mutation
% --------------------------------------------------
% TODO: Change title to something more appropriate

%% hillclimber(Mutation, Fitness, Individual, Best)
% TODO: Document

:- meta_predicate hillclimber(2, 2, +, -).

hillclimber(Mutation, Fitness, Individual, BestMutant) :-
	debug(hillclimber, 'mutating ~w', [Individual]),
	call(Fitness, Individual, BaseFitness),
	findall(Mutant, (
		call(Mutation, Individual, Mutant),
		call(Fitness, Mutant, MutantFitness),
		MutantFitness > BaseFitness
	), MutationPool),
	mergesort(MutationPool, descending(Fitness), [CurrentBestMutant|_]),
	hillclimber(Mutation, Fitness, CurrentBestMutant, BestMutant).

hillclimber(_, _, X, X) :-
	debug(hillclimber, 'DONE: chose ~w', X).


%% population_hillclimber(+Mutation, +Fitness, +Population, -Mutants)
% TODO: Document

:- meta_predicate population_hillclimber(2, 2, +, -).

population_hillclimber(Mutation, Fitness, Population, Mutants) :-
	debug(hillclimber, 'starting population_hillclimber', []),
	findall(ID, (
		member(X, Population),
		thread_create((
			once(hillclimber(Mutation, Fitness, X, Mutant)),
			thread_exit(Mutant)
		), ID, [])
	), ThreadIDs),
	findall(Mutant, (
		member(ID, ThreadIDs),
		thread_join(ID, exited(Mutant))
	), Mutants),
	debug(hillclimber, 'DONE with population_hillclimber', []).
