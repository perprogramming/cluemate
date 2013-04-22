:- module(rooms_model, [
	create_room/2,
	get_room_name/2,
	is_room_suspicious/1,
	set_room_suspicious/3
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
	
set_room_suspicious(Room, Suspicious, NewRoom) :-
	set_suspicious_of_room(Suspicious, Room, NewRoom).