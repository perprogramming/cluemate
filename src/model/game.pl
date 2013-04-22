:- module(game_model, [
	is_game/1,
	create_game/1,
	can_start_game/1,
	is_game_started/1,
	start_game/2,
	can_end_game/1,
	end_game/2,
	can_add_person/1,
	add_person/3,
	add_persons/3,
	get_persons/2,
	can_add_weapon/1,
	add_weapon/3,
	add_weapons/3,
	get_weapons/2,
	can_add_room/1,
	add_room/3,
	add_rooms/3,
	get_rooms/2,
	get_suspicious_rooms/2,
	get_room_by_name/3,
	set_current_room/3,
	get_current_room/2,
	is_curret_room/2,
	get_next_room/2
]).

:- use_module(library(record)).
:- use_module(library(lists)).
:- use_module('util').
:- use_module('persons').
:- use_module('weapons').
:- use_module('rooms').

:- record game(
	started:boolean=false,
	persons:list=[],
	weapons:list=[],
	rooms:list=[],
	currentroom=false
).

is_game(false) :-
	!, fail.
		
is_game(Game).

create_game(Game) :-
	default_game(Game).

can_start_game(Game) :-
	is_game(Game),
	not(is_game_started(Game)).
	
can_end_game(Game) :-
	is_game(Game).
	
is_game_started(Game) :-
	game_started(Game, true). 

start_game(Game, NewGame) :-
	set_started_of_game(true, Game, NewGame).

end_game(Game, false).
	
can_add_person(Game) :-
	can_start_game(Game).
	
add_persons(Game, [], Game) :-
	!.

add_persons(Game, Names, NewGame) :-
	nth0(0, Names, Name, RemainingNames),
	add_person(Game, Name, NextGame),
	add_persons(NextGame, RemainingNames, NewGame).

add_person(Game, Name, NewGame) :-
	create_person(Person, Name),
	game_persons(Game, Persons),
	append(Persons, [Person], NewPersons),
	set_persons_of_game(NewPersons, Game, NewGame).
	
get_persons(Game, Persons) :-
	game_persons(Game, Persons).
	
can_add_weapon(Game) :-
	can_start_game(Game).
	
add_weapons(Game, [], Game) :-
	!.

add_weapons(Game, Names, NewGame) :-
	nth0(0, Names, Name, RemainingNames),
	add_weapon(Game, Name, NextGame),
	add_weapons(NextGame, RemainingNames, NewGame).

add_weapon(Game, Name, NewGame) :-
	create_weapon(Weapon, Name),
	game_weapons(Game, Weapons),
	append(Weapons, [Weapon], NewWeapons),
	set_weapons_of_game(NewWeapons, Game, NewGame).
	
get_weapons(Game, Weapons) :-
	game_weapons(Game, Weapons).

can_add_room(Game) :-
	can_start_game(Game).
	
add_rooms(Game, [], Game) :-
	!.

add_rooms(Game, Names, NewGame) :-
	nth0(0, Names, Name, RemainingNames),
	add_room(Game, Name, NextGame),
	add_rooms(NextGame, RemainingNames, NewGame).

add_room(Game, Name, NewGame) :-
	create_room(Room, Name),
	game_rooms(Game, Rooms),
	append(Rooms, [Room], NewRooms),
	set_rooms_of_game(NewRooms, Game, NewGame).
	
get_rooms(Game, Rooms) :-
	game_rooms(Game, Rooms).

get_suspicious_rooms(Game, SuspiciousRooms) :-
	get_rooms(Game, Rooms),
	filter_list(rooms_model:is_room_suspicious, Rooms, SuspiciousRooms).
	
set_current_room(Game, Room, NewGame) :-
	set_currentroom_of_game(Room, Game, NewGame).

get_room_by_name(Game, Name, Room) :-
	get_rooms(Game, Rooms),
	find_list_element(game_model:compare_room_name(Name), Rooms, Room),
	!.
	
get_room_by_name(Game, Name, false).

compare_room_name(Name, Room) :-
	get_room_name(Room, Name),
	!.
	
compare_room_name(Name, Room) :-
	false,
	!.

get_current_room(Game, Room) :-
	game_currentroom(Game, Room).
	
is_current_room(Game, Room) :-
	game_currentroom(Game, Room).
	
get_next_room(Game, Room) :-
	get_suspicious_rooms(Game, SuspiciousRooms),
	nth0(0, SuspiciousRooms, Room),
	not(Room=false).
	
get_next_room(Game, Room) :-
	get_rooms(Game, Rooms),
	nth0(0, Rooms, Room).