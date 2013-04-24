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
	get_suspicious_persons/2,
	mark_person_by_name/3,
	get_murder/2,
	can_add_weapon/1,
	add_weapon/3,
	add_weapons/3,
	get_weapons/2,
	get_suspicious_weapons/2,
	mark_weapon_by_name/3,
	get_murder_weapon/2,
	can_add_room/1,
	add_room/3,
	add_rooms/3,
	get_rooms/2,
	get_suspicious_rooms/2,
	set_current_room_by_name/3,
	get_current_room/2,
	get_next_room/2,
	mark_room_by_name/3,
	get_crime_scene/2,
	get_most_suspicious_person/2,
	get_most_suspicious_weapon/2,
	get_most_suspicious_room/2
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
	not(is_game_started(Game)),
	get_persons(Game, Persons),
	proper_length(Persons, NumberOfPersons),
	NumberOfPersons > 1,
	get_weapons(Game, Weapons),
	proper_length(Weapons, NumberOfWeapons),
	NumberOfWeapons > 1,
	get_rooms(Game, Rooms),
	proper_length(Rooms, NumberOfRooms),
	NumberOfRooms > 1.
	
can_end_game(Game) :-
	is_game(Game).
	
is_game_started(Game) :-
	game_started(Game, true). 

start_game(Game, NewGame) :-
	set_started_of_game(true, Game, NewGame).

end_game(Game, false).
	
can_add_person(Game) :-
	is_game(Game),
	not(is_game_started(Game)).
	
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
	sort(NewPersons, SortedPersons),
	set_persons_of_game(SortedPersons, Game, NewGame).
	
get_persons(Game, Persons) :-
	game_persons(Game, Persons).

get_suspicious_persons(Game, SuspiciousPersons) :-
	get_persons(Game, Persons),
	filter_list(persons_model:is_person_suspicious, Persons, SuspiciousPersons).

mark_person_by_name(Game, Name, NewGame) :-
	get_persons(Game, Persons),
	find_list_element(game_model:compare_person_name(Name), Persons, Person),
	exclude(=(Person), Persons, OtherPersons),
	set_person_suspicious(Person, false, UnsuspiciousPerson),
	append(OtherPersons, [UnsuspiciousPerson], NewPersons),
	sort(NewPersons, SortedPersons),
	set_persons_of_game(SortedPersons, Game, NewGame).

mark_person_by_name(Game, Name, NewGame) :-
	!,
	false.

compare_person_name(Name, Person) :-
	get_person_name(Person, Name),
	!.
	
compare_person_name(Name, Person) :-
	false,
	!.

get_murder(Game, Murder) :-
	get_suspicious_persons(Game, SuspiciousPersons),
	proper_length(SuspiciousPersons, 1),
	nth0(0, SuspiciousPersons, Murder),
	!.

get_murder(Game, false) :-
	!.
	
can_add_weapon(Game) :-
	is_game(Game),
	not(is_game_started(Game)).
	
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
	sort(NewWeapons, SortedWeapons),
	set_weapons_of_game(SortedWeapons, Game, NewGame).
	
get_weapons(Game, Weapons) :-
	game_weapons(Game, Weapons).

get_suspicious_weapons(Game, SuspiciousWeapons) :-
	get_weapons(Game, Weapons),
	filter_list(weapons_model:is_weapon_suspicious, Weapons, SuspiciousWeapons).
	
get_murder_weapon(Game, MurderWeapon) :-
	get_suspicious_weapons(Game, SuspiciousWeapons),
	proper_length(SuspiciousWeapons, 1),
	nth0(0, SuspiciousWeapons, MurderWeapon),
	!.

mark_weapon_by_name(Game, Name, NewGame) :-
	get_weapons(Game, Weapons),
	find_list_element(game_model:compare_weapon_name(Name), Weapons, Weapon),
	exclude(=(Weapon), Weapons, OtherWeapons),
	set_weapon_suspicious(Weapon, false, UnsuspiciousWeapon),
	append(OtherWeapons, [UnsuspiciousWeapon], NewWeapons),
	sort(NewWeapons, SortedWeapons),
	set_weapons_of_game(SortedWeapons, Game, NewGame).

mark_weapon_by_name(Game, Name, NewGame) :-
	!,
	false.

compare_weapon_name(Name, Weapon) :-
	get_weapon_name(Weapon, Name),
	!.
	
compare_weapon_name(Name, Weapon) :-
	false,
	!.

get_murder_weapon(Game, false) :-
	!.
	
can_add_room(Game) :-
	is_game(Game),
	not(is_game_started(Game)).
	
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
	sort(NewRooms, SortedRooms),
	set_rooms_of_game(SortedRooms, Game, NewGame).
	
get_rooms(Game, Rooms) :-
	game_rooms(Game, Rooms).

get_suspicious_rooms(Game, SuspiciousRooms) :-
	get_rooms(Game, Rooms),
	filter_list(rooms_model:is_room_suspicious, Rooms, SuspiciousRooms).
	
set_current_room_by_name(Game, Name, NewGame) :-
	get_rooms(Game, Rooms),
	find_list_element(game_model:compare_room_name(Name), Rooms, Room),
	set_currentroom_of_game(Room, Game, NewGame),
	!.
	
set_current_room_by_name(Game, Name, false) :-
	!,
	false.

compare_room_name(Name, Room) :-
	get_room_name(Room, Name),
	!.
	
compare_room_name(Name, Room) :-
	false,
	!.

get_current_room(Game, Room) :-
	game_currentroom(Game, Room).
	
get_next_room(Game, Room) :-
	get_suspicious_rooms(Game, SuspiciousRooms),
	nth0(0, SuspiciousRooms, Room).
	
mark_room_by_name(Game, Name, NewGame) :-
	get_rooms(Game, Rooms),
	find_list_element(game_model:compare_room_name(Name), Rooms, Room),
	exclude(=(Room), Rooms, OtherRooms),
	set_room_suspicious(Room, false, UnsuspiciousRoom),
	append(OtherRooms, [UnsuspiciousRoom], NewRooms),
	sort(NewRooms, SortedRooms),
	set_rooms_of_game(SortedRooms, Game, NewGame).

mark_room_by_name(Game, Name, NewGame) :-
	!,
	false.

get_crime_scene(Game, CrimeScene) :-
	get_suspicious_rooms(Game, SuspiciousRooms),
	proper_length(SuspiciousRooms, 1),
	nth0(0, SuspiciousRooms, CrimeScene),
	!.
	
get_crime_scene(Game, false) :-
	!.
	
get_most_suspicious_person(Game, Person) :-
	!,
	get_suspicious_persons(Game, SuspiciousPersons),
	nth0(0, SuspiciousPersons, Person).
	
get_most_suspicious_weapon(Game, Weapon) :-
	!,
	get_suspicious_weapons(Game, SuspiciousWeapons),
	nth0(0, SuspiciousWeapons, Weapon).
	
get_most_suspicious_room(Game, Room) :-
	!,
	get_suspicious_rooms(Game, SuspiciousRooms),
	nth0(0, SuspiciousRooms, Room).
