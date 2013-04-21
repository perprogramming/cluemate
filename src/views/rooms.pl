:- module(rooms_views, [
	render_rooms/1
]).

:- use_module(library(lists)).
:- use_module('../model/rooms').
:- use_module('util').

render_rooms([]) :-
	!.
	
render_rooms(Rooms) :-
	render_list('Rooms', rooms_model:get_room_name, Rooms).