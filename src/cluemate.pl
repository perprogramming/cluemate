:- module(cluemate, [
	cluemate/0
]).

:- use_module('model/game').
:- use_module('views/game').
:- use_module('actions/game').

cluemate :-
	mainloop(false, 'Welcome to Cluemate', false).
		
mainloop(Game, Flash, Exit) :-
	tty_clear,
	render_game(Game, Flash, Exit),
	
	(not(Exit) ; halt),
	
	nl,
	prompt1('Please enter the next action: '),
	read(Action),	
	
	execute_game_action(Action, Game, NewGame, NewFlash, NewExit),
	writeln(NewExit),
	mainloop(NewGame, NewFlash, NewExit).
