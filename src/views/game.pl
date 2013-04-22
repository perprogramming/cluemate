:- module(game_views, [
	render_game/3
]).

:- use_module('../model/game').
:- use_module('../model/rooms').
:- use_module('actions').
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
		render_current_room(Game),
		nl,
		render_available_items(Game),
		render_suspicious_items(Game),
		render_suggestions(Game),
		render_actions(Game)
	)).
	
render_game_status(Game) :-
	not(is_game(Game)),
	!,
	tab(2), writeln('You have not created a game yet.').
	
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

render_current_room(Game) :-
	is_game(Game),
	is_game_started(Game),
	get_current_room(Game, false),
	!,
	tab(2), writeln('You are not in a room.').
	
render_current_room(Game) :-
	is_game(Game),
	is_game_started(Game),
	!,
	get_current_room(Game, CurrentRoom),
	get_room_name(CurrentRoom, Name),
	tab(2), write('You are in room '), write(Name), write('.'),
	nl.
	
render_current_room(Game) :-
	!.

render_available_items(Game) :-
	is_game(Game),
	!,
	tab(2), writeln('AVAILABLE'), 
	get_persons(Game, Persons),	
	render_list('Persons', persons_model:get_person_name, Persons),
	get_weapons(Game, Weapons),
	render_list('Weapons', weapons_model:get_weapon_name, Weapons),
	get_rooms(Game, Rooms),
	render_list('Rooms', rooms_model:get_room_name, Rooms),
	nl.

render_available_items(Game) :-
	!.
	
render_suspicious_items(Game) :-
	is_game(Game),
	is_game_started(Game),
	!,
	tab(2), writeln('SUSPICIOUS'),
	get_persons(Game, SuspiciousPersons),	
	render_list('Persons', persons_model:get_person_name, SuspiciousPersons),
	get_weapons(Game, SuspiciousWeapons),
	render_list('Weapons', weapons_model:get_weapon_name, SuspiciousWeapons),
	get_suspicious_rooms(Game, SuspiciousRooms),
	render_list('Rooms', rooms_model:get_room_name, SuspiciousRooms),
	nl.
	
render_suspicious_items(Game) :-
	!.
	
render_suggestions(Game) :-
	is_game(Game),
	is_game_started(Game),
	!,
	tab(2), writeln('SUGGESTIONS').	
	
	
render_suggestions(Game) :-
	!.
	