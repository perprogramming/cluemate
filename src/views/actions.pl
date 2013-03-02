:- module(actions_views, [
	render_actions/1
]).

:- use_module('../actions/game').

render_actions(Game) :-
	!,
	tab(2), writeln('Available actions:'),
	(render_action_offers(Game) ; true).
	
render_action_offers(Game) :-
	offer_game_action(Action, Game, Description),
	tab(2), write(Action), write(' => '), write(Description),
	nl,
	fail.
