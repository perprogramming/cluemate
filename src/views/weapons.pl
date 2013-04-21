:- module(weapons_views, [
	render_weapons/1
]).

:- use_module(library(lists)).
:- use_module('../model/weapons').
:- use_module('util').

render_weapons([]) :-
	!.
	
render_weapons(Weapons) :-
	render_list('Weapons', weapons_model:get_weapon_name, Weapons).