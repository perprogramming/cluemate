:- module(cluemate, [
	cluemate/0
]).

:- use_module('model/game').
:- use_module('views/game').
:- use_module('actions/game').
:- use_module('actions/util').

cluemate :-
	mainloop(false, 'Welcome to Cluemate', false),
	halt.
		
mainloop(Game, Flash, Exit) :-
	render_game(Game, Flash),
	(Exit ; (
		read_user_input('Please enter the next action: ', Action),
		execute_game_action(Action, Game, NewGame, NewFlash, NewExit),
		mainloop(NewGame, NewFlash, NewExit)
	)).
