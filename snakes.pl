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

:- module(snakes, [

	sorting_test/0,
	main/0

]).

:- use_module('lib/graph2').
:- use_module('lib/sort').
:- use_module('lib/search/genetic').


% This predicate is the logger/terminating condition for the GA
% When it succeeds, the GA stops
terminate(Dimension, N, Population, Soln) :-
	sort(Population, SortedPopulation), % remove dupes

	mergesort(SortedPopulation, descending(fitness(Dimension)), [Best|_]),
	snake(Dimension, Best, BestSnake),
	length(BestSnake, L),
	L0 is L-1,

	length(SortedPopulation, PopSize),

	statistics(walltime, [_, MilliTime]),
	Time is MilliTime / 1000,

	format('~w,~w,~w,~w\n', [N,Time,PopSize,L0]),
	cleanup_auto(10),

	!,

	% Stop when the population becomes small
	PopSize =< 8,
	Soln = BestSnake.


% Experiments
% --------------------------------------------------

% An experiment on sorting algorithms
% Results: Mergesort is faster
sorting_test :-
	Dimension = 5,
	Trials = 10, % the test is not tail recursive, don't make this too big
	format('Sorting ~d random populations in dimension ~d\n', [Trials, Dimension]),
	sorting_test_(Dimension, Trials, QuickTime, MergeTime),
	MeanQuickTime is QuickTime / Trials,
	MeanMergeTime is MergeTime / Trials,
	format('quicksort: ~0fms\n', [MeanQuickTime]),
	format('mergesort: ~0fms\n', [MeanMergeTime]).

sorting_test_(_, 0, 0, 0) :- !.
sorting_test_(Dimension, Trials, QuickTime, MergeTime) :-
	!,
	genetic:random_population(Dimension, 1, Pop),

	genetic:cleanup,
	statistics(walltime, _),
	quicksort(Pop, descending(genetic:fitness(Dimension)), _),
	statistics(walltime, [_, ThisQuickTime]),

	genetic:cleanup,
	statistics(walltime, _),
	mergesort(Pop, descending(genetic:fitness(Dimension)), _),
	statistics(walltime, [_, ThisMergeTime]),

	NextTrials is Trials - 1,
	sorting_test_(Dimension, NextTrials, NextQuickTime, NextMergeTime),
	QuickTime is NextQuickTime + ThisQuickTime,
	MergeTime is NextMergeTime + ThisMergeTime.


% Find the longest snake in 7D using the GA
% This will run indefinitly, printing results after each generation
main :-

	debug(genetic_search),
	% debug(hillclimber),
	% debug(mergesort),

	Dimension = 8,

	PopulationSize = 100,

	Fitness = fitness(Dimension),
	Select = rank_select(80),
	Reproduce = grow_population(Dimension),
	Mutate = population_hillclimber(xor_mutant),
	Terminate = terminate(Dimension),

	random_population(Dimension, PopulationSize, InitialPopulation),
	genetic_search(InitialPopulation, Fitness, Select, Reproduce, Mutate, Terminate, _),

	halt.
