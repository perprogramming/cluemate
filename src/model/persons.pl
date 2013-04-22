:- module(persons_model, [
	create_person/2,
	get_person_name/2,
	is_person_suspicious/1,
	set_person_suspicious/3
]).

:- use_module(library(record)).

:- record person(
	name:text='',
	suspicious:boolean=true
).

create_person(Person, Name) :-
	make_person([
		name(Name)
	], Person).
		
get_person_name(Person, Name) :-
	person_name(Person, Name).

is_person_suspicious(Person) :-
	person_suspicious(Person, true).
	
set_person_suspicious(Person, Suspicious, NewPerson) :-
	set_suspicious_of_person(Suspicious, Person, NewPerson).