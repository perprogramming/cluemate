:- module(util_actions, [
	read_user_input/2
]).

read_user_input(Question, Answer) :-
	tab(2), prompt1(Question),
	read(Answer).