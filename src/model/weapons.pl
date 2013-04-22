:- module(weapons_model, [
	create_weapon/2,
	get_weapon_name/2,
	is_weapon_suspicious/1,
	set_weapon_suspicious/3
]).

:- use_module(library(record)).

:- record weapon(
	name:text='',
	suspicious:boolean=true
).

create_weapon(Weapon, Name) :-
	make_weapon([
		name(Name)
	], Weapon).
		
get_weapon_name(Weapon, Name) :-
	weapon_name(Weapon, Name).

is_weapon_suspicious(Weapon) :-
	weapon_suspicious(Weapon, true).
	
set_weapon_suspicious(Weapon, Suspicious, NewWeapon) :-
	set_suspicious_of_weapon(Suspicious, Weapon, NewWeapon).