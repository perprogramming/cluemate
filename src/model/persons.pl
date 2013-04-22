:- module(persons_model, [
	create_person/2,
	get_person_name/2
]).

:- use_module(library(record)).

:- record person(
	name:text='',
	seen:boolean=false
).

create_person(Person, Name) :-
	make_person([
		name(Name)
	], Person).
		
get_person_name(Person, Name) :-
	person_name(Person, Name).
