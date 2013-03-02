:- module(util, [
	render_line/1
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
    