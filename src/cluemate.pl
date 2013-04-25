:- module(cluemate, [
	cluemate/0
]).

:- use_module('model/game').
:- use_module('views/game').
:- use_module('actions/game').

cluemate :-
	mainloop(false, 'Welcome to Cluemate', false),
	halt.
		
mainloop(Game, Flash, Exit) :-
	tty_clear,
	render_game(Game, Flash),
	nl,
	
	(Exit ; (
		tab(2), prompt1('Please enter the next action: '),
		read(Action),	
	
		execute_game_action(Action, Game, NewGame, NewFlash, NewExit),
		mainloop(NewGame, NewFlash, NewExit)
	)).
