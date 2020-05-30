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

list_mapSideEffect(_, []).
list_mapSideEffect(Func, [L|Ls]) :-
    call(Func, L),
    list_mapSideEffect(Func, Ls).