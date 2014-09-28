:- module(naive, [

	long_snake/2

]).

:- use_module('lib/graph').


%% long_snake(+Dimension, -Snake)
% Implements an iterative deepening search for a long snake.

long_snake(Dimension, Snake) :-
	format('iterative deepening search\n', []),
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
