:- module(weapons_model, [
	create_weapon/2,
	get_weapon_name/2
]).

:- use_module(library(record)).

:- record weapon(
	name:text='',
	seen:boolean=false
).

create_weapon(Weapon, Name) :-
	make_weapon([
		name(Name)
	], Weapon).
		
get_weapon_name(Weapon, Name) :-
	weapon_name(Weapon, Name).
