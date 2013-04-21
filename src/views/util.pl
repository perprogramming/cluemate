:- module(util, [
	render_line/1,
	render_list/3
]).

render_line(Char) :-
	tty_size(_, NumberOfColumns),
	render_repeated_char(Char, NumberOfColumns),
	nl.
	
render_repeated_char(Char, 0) :-
	!.

render_repeated_char(Char, Count) :-
	write(Char),
	NextCount is Count-1,
	render_repeated_char(Char, NextCount).
    
render_list(Label, ElementLabelGoal, List) :-
	tab(2), write(Label), write(': '),
	render_list_recursively(ElementLabelGoal, List),
	nl.

render_list_recursively(ElementLabelGoal, []) :-
	!.

render_list_recursively(ElementLabelGoal, [LastElement]) :-
	call(ElementLabelGoal, LastElement, ElementLabel), 
	write(ElementLabel).	

render_list_recursively(ElementLabelGoal, List) :-
	nth0(0, List, Element, RemainingList),
	render_list_recursively(ElementLabelGoal, [Element]),
	write(', '),
	render_list_recursively(ElementLabelGoal, RemainingList).
    