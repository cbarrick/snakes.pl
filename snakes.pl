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
	selection_bias_test/0,
	main/0

]).

:- use_module('lib/graph').
:- use_module('lib/sort').
:- use_module('lib/search/naive').
:- use_module('lib/search/genetic').


% Experiments
% --------------------------------------------------

% An experiment on sorting algorithms
% Results: Mergesort is faster
sorting_test :-
	D = 6,
	genetic:random_population(D, 1, Pop),
	length(Pop, N),

	format('Sorting ~w random paths in dimension ~w\n', [N, D]),

	format('\nquicksort:\n'),
	genetic:cleanup,
	time(quicksort(Pop, descending(genetic:fitness(D)), Sorted1)),

	format('\nmergesort\n'),
	genetic:cleanup,
	time(mergesort(Pop, descending(genetic:fitness(D)), Sorted2)),

	% Check that the lists are equivalent
	% The sorted lists may not be in the same order given that some paths
	% have the same fitness. So we must check against the fitness of the
	% paths in the lists.
	findall(X, (
		member(A, Sorted1),
		genetic:fitness(D, A, X)
	), Xs),
	findall(Y, (
		member(A, Sorted2),
		genetic:fitness(D, A, Y)
	), Ys),
	format('~w\n', Xs),
	format('~w\n', Ys),
	Xs = Ys.


% An experiment on selection weights
% Results: The weight 1/n will select the nth element on average
% with a standard deviation of n.
selection_bias_test :-
	findall(P, between(1, 1000, P), Population),
	length(Population, PopulationSize),
	format('population size: ~w\n', [PopulationSize]),

	TrialSize = 1000,
	format('trail size: ~w\n', [TrialSize]),

	write('-----\n'),

	findall(X, (between(1, 10, Y), (X is Y/10 ; X is Y/100 ; X is Y/1000)), Trials),
	% Trials = [0.01, 0.02, 0.03, 0.1, 0.2, 0.3, 1],

	forall( member(Bias, Trials), (
		format('weight: ~w\n', [Bias]),

		findall(S, (
			between(1, TrialSize, _),
			genetic:biased_select(Bias, S, Population, _)
		), Ss ),

		sum_list(Ss, Sum),
		Mean is Sum / TrialSize,
		format('  mean: ~w\n', [Mean]),

		findall(X, (member(X_, Ss), X is (X_ - Mean) ^ 2), Xs),
		sum_list(Xs, Y),
		StandardDeviation is sqrt(Y / TrialSize),
		format('  standard deviation: ~w\n', [StandardDeviation])
	)).


% Find the longest snake in 7D using the GA
% This will run indefinitly, printing results after each generation
main :-

	% The longest snake in 7D is 51 nodes.
	Dimension = 7,

	% During mate selection, we use a modified roulette select. This determines
	% how strongly better solutions should be favored. Higher values quickly
	% reduse the set of likely candidates. A value of 'auto' will enable
	% dynamic selection bias.
	SelectionBias = auto,

	% When breading, not all possible children are returned. Some children "die"
	% before becoming productive. This determines how many children survive.
	% Lower rates mean each generation is faster, higher rates mean each
	% generation is better. The goal is to optimize execution speed.
	SurvivalRate = 0.75,

	% Mutations stack: at 0.1, each solution has a 10% chance to mutate once,
	% a 1% chance to mutate twice, a 0.1% chance to mutate 3 times, etc.
	MutationRate = 0.1,

	genetic:long_snake(Dimension, 1, SelectionBias, SurvivalRate, MutationRate, _).
