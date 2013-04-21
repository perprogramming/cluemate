:- module(persons_views, [
	render_persons/1
]).

:- use_module(library(lists)).
:- use_module('../model/persons').
:- use_module('util').

render_persons([]) :-
	!.

render_persons(Persons) :-
	render_list('Persons', persons_model:get_person_name, Persons).	