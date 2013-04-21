:- module(rooms_model, [
	create_room/2,
	get_room_name/2
]).

:- use_module(library(record)).

:- record room(
	name:text=''
).

create_room(Room, Name) :-
	make_room([
		name(Name)
	], Room).
		
get_room_name(Room, Name) :-
	room_name(Room, Name).
