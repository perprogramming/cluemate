:- module(game_actions, [
	offer_game_action/3,
	execute_game_action/5
]).

:- use_module('../model/game').

offer_game_action('classic', Game, 'Create and start a classic game').

offer_game_action('c', Game, 'Create a new game').

offer_game_action('s', Game, 'Start the game') :-
	can_start_game(Game).
	
offer_game_action('ap', Game, 'Add a person') :-
	can_add_person(Game).
	
offer_game_action('x', Game, 'Exit').

execute_game_action('classic', Game, NewGame, 'You have just created and started a classic game!', false) :-
	!,
	create_game(PlainGame),
	add_persons(PlainGame, [
		'Colonel Mustard',
		'Professor Plum',
		'Reverend Green',
		'Mrs. Peacock',
		'Miss Scarlett',
		'Mrs. White'
	], GameWithPersons),
	start_game(GameWithPersons, NewGame).

execute_game_action('c', Game, NewGame, 'You have just created a new game!', false) :-
	!,
	create_game(NewGame).

execute_game_action('s', Game, NewGame, 'You have just started the game!', false) :-
	can_start_game(Game),
	!,
	start_game(Game, NewGame).

execute_game_action('ap', Game, NewGame, Flash, false) :-
	can_add_person(Game),
	!,
	prompt1("Please enter the name of the person: "),
	read(Name),
	add_person(Game, Name, NewGame),
	string_concat('You have just added Person ', Name, Temp),
	string_concat(Temp, '!', Flash).

execute_game_action('x', Game, Game, 'Thanks for using Cluemate and Goodbye!', true) :-
	!.
	
execute_game_action(_, Game, Game, 'You have entered an unknown or invalid action!', false).