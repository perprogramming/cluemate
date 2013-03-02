:- module(game_views, [
	render_game/3
]).

:- use_module('../model/game').
:- use_module('actions').
:- use_module('persons').
:- use_module('util').

render_game(Game, Flash, Exit) :-
	!,
	nl,
	tab(2), writeln('C L U E M A T E'),
	render_line('-'),
	
	tab(2), writeln(Flash),
	render_line('-'),
	nl,
	
	(Exit ; (

		render_game_status(Game),
		render_game_details(Game),
		nl,
	
		render_actions(Game)
	)).
	
render_game_status(Game) :-
	is_game(Game),	
	not(is_game_started(Game)),
	!,
	tab(2), writeln('The game is not started.').
	
render_game_status(Game) :-
	is_game(Game),
	is_game_started(Game),
	!,
	tab(2), writeln('The game is ongoing.').

render_game_status(Game) :-
	!.

render_game_details(Game) :-
	is_game(Game),
	!,
	get_persons(Game, Persons),
	render_persons(Persons).
	
render_game_details(Game) :-
	!.
	