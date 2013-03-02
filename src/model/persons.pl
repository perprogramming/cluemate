:- module(persons_model, [
	create_person/2,
	get_name/2,
	is_player/1
]).

:- use_module(library(record)).

:- record person(
	name:text=''
).

create_person(Person, Name) :-
	make_person([
		name(Name)
	], Person).
		
get_name(Person, Name) :-
	person_name(Person, Name).
