:- module(persons_views, [
	render_persons/1
]).

:- use_module(library(lists)).
:- use_module('../model/persons').

render_persons([]) :-
	!.

render_persons(Persons) :-
	tab(2), writeln('Persons:'),
	render_persons_recursively(Persons).

render_persons_recursively([]) :-
	!.

render_persons_recursively(Persons) :-
	nth0(0, Persons, Person, RemainingPersons),
	get_name(Person, Name),
	tab(2), writeln(Name),
	render_persons_recursively(RemainingPersons).
