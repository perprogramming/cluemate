:- module(util_model, [
	filter_list/3,
	find_list_element/3
]).

:- use_module(library(lists)).

filter_list(Goal, List, FilteredList) :-
	exclude(Goal, List, NotMatchingList),
	subtract(List, NotMatchingList, FilteredList).
	
find_list_element(Goal, List, Element) :-
	filter_list(Goal, List, FilteredList),
	nth0(0, FilteredList, Element).
	