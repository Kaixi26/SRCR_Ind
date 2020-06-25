list_filter(_,[],[]).
list_filter(Filter, [X|Ls], [X|Ans]) :-
    call(Filter, X),
    list_filter(Filter, Ls, Ans).
list_filter(Filter, [X|Ls], Ans) :-
    \+ call(Filter, X),
    list_filter(Filter, Ls, Ans).

list_dropAnyN(Ans, Ans).
list_dropAnyN([_|Ls], Ans) :-
    list_dropAnyN(Ls, Ans).

list_takeAnyN(_, []).
list_takeAnyN([A|Ls], [A|Ans]) :-
    list_takeAnyN(Ls, Ans).

list_elem([], _) :- fail.
list_elem([Elem|_], Elem).
list_elem([_|Ls], Elem) :- list_elem(Ls, Elem).

list_remove1(_, [], []).
list_remove1(V, [V|Ls], Ls).
list_remove1(V, [L|Ls], [L|Ans]) :- 
    V \= L,
    list_remove1(V, Ls, Ans).

list_init([], []).
list_init([_], []).
list_init([L|Ls], [L|LInit]) :-
    \+ Ls = [],
    list_init(Ls, LInit).

list_append([], List, List).
list_append([L|Ls], List, [L|SubLs]) :-
    list_append(Ls, List, SubLs).
