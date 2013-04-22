:- module(rooms_model, [
	create_room/2,
	get_room_name/2
]).

:- use_module(library(record)).

:- record room(
	name:text='',
	suspicious:boolean=true
).

create_room(Room, Name) :-
	make_room([
		name(Name)
	], Room).
		
get_room_name(Room, Name) :-
	room_name(Room, Name).
	
is_room_suspicious(Room) :-
	room_suspicious(Room, true).