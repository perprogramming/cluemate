:- module(game_actions, [
	offer_game_action/3,
	execute_game_action/5
]).

:- use_module('../model/game').

offer_game_action('classic', Game, 'Create and start a classic game') :-
	not(is_game(Game)).

offer_game_action('c', Game, 'Create a new game') :-
	not(is_game(Game)).

offer_game_action('s', Game, 'Start the game') :-
	can_start_game(Game).

offer_game_action('ap', Game, 'Add a person') :-
	can_add_person(Game).
	
offer_game_action('aw', Game, 'Add a weapon') :-
	can_add_weapon(Game).

offer_game_action('ar', Game, 'Add a room') :-
	can_add_room(Game).

offer_game_action('g', Game, 'Go to room') :-
	is_game_started(Game).

offer_game_action('e', Game, 'End the game') :-
	is_game(Game),
	can_end_game(Game).
		
offer_game_action('x', Game, 'Exit').

execute_game_action('classic', Game, NewGame, 'You have just created and started a classic game!', false) :-
	!,
	create_game(PlainGame),
	add_persons(PlainGame, [
		'Oberst von Gatow',
		'Fraeulein Ming',
		'Professor Bloom',
		'Herr Direktor Gruen',
		'Frau Weiss',
		'Baronin von Porz'
	], GameWithPersons),
	add_weapons(GameWithPersons, [
		'Pistole',
		'Dolch',
		'Seil',
		'Kerzenleuchter',
		'Rohrzange',
		'Heizungsrohr'
	], GameWithWeapons),
	add_rooms(GameWithWeapons, [
		'Kueche',
		'Musikzimmer',
		'Veranda',
		'Billardzimmer',
		'Bibliothek',
		'Arbeitszimmer',
		'Eingangshalle',
		'Saloon',
		'Speisezimmer'
	], GameWithRooms),
	start_game(GameWithRooms, NewGame).

execute_game_action('c', Game, NewGame, 'You have just created a new game!', false) :-
	!,
	create_game(NewGame).

execute_game_action('s', Game, NewGame, 'You have just started the game!', false) :-
	can_start_game(Game),
	!,
	start_game(Game, NewGame).

execute_game_action('e', Game, NewGame, 'You have just stopped the game!', false) :-
	can_end_game(Game),
	!,
	end_game(Game, NewGame).

execute_game_action('ap', Game, NewGame, Flash, false) :-
	can_add_person(Game),
	!,
	prompt1('Please enter the name of the person (in quotes): '),
	read(Name),
	add_person(Game, Name, NewGame),
	string_concat('You have just added person ', Name, Temp),
	string_concat(Temp, '!', Flash).
	
execute_game_action('aw', Game, NewGame, Flash, false) :-
	can_add_weapon(Game),
	!,
	prompt1('Please enter the name of the weapon (in quotes): '),
	read(Name),
	add_weapon(Game, Name, NewGame),
	string_concat('You have just added weapon ', Name, Temp),
	string_concat(Temp, '!', Flash).

execute_game_action('ar', Game, NewGame, Flash, false) :-
	can_add_weapon(Game),
	!,
	prompt1('Please enter the name of the room (in quotes): '),
	read(Name),
	add_room(Game, Name, NewGame),
	string_concat('You have just added room ', Name, Temp),
	string_concat(Temp, '!', Flash).

execute_game_action('g', Game, NewGame, Flash, false) :-
	is_game_started(Game),
	!,
	prompt1('Please enter the name of the room (in quotes): '),
	read(Name),
	get_room_by_name(Game, Name, Room),
	((
		Room=false,
		Flash='Room not found!',
		NewGame=Game
	 ) ; (
		set_current_room(Game, Room, NewGame),
		string_concat('You have just gone to room ', Name, Temp),
		string_concat(Temp, '!', Flash)
	)).

execute_game_action('x', Game, Game, 'Thanks for using Cluemate and Goodbye!', true) :-
	!.
	
execute_game_action(_, Game, Game, 'You have entered an unknown or invalid action!', false).