:- module(game_model, [
	is_game/1,
	create_game/1,
	can_start_game/1,
	is_game_started/1,
	can_add_person/1,
	add_person/3,
	add_persons/3,
	start_game/2,
	get_persons/2
]).

:- use_module(library(record)).
:- use_module(library(lists)).
:- use_module('persons').

:- record game(
	started:boolean=false,
	persons:list=[]
).

is_game(false) :-
	!, fail.
		
is_game(Game).

create_game(Game) :-
	default_game(Game).

can_start_game(Game) :-
	is_game(Game),
	not(is_game_started(Game)).
	
is_game_started(Game) :-
	game_started(Game, true). 
	
can_add_person(Game) :-
	can_start_game(Game).
	
start_game(Game, NewGame) :-
	set_started_of_game(true, Game, NewGame).

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
	set_persons_of_game(NewPersons, Game, NewGame).
	
get_persons(Game, Persons) :-
	game_persons(Game, Persons).