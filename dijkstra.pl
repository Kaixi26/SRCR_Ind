%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% DIJKSTRA ALGORITHM %%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%test(Ans) :- dijk_solve([(1,(0,-1))], 2, [(1,2,1), (1,3,1)], Ans).
%
%test2(Ans) :-
%    findall((X,Y, 1), adjacencia(_, X, Y), Adj),
%    dijk_solve([(183, (0, -1))], 611, Adj, Ans).

dijk_solve(Visited, End, _, Visited) :-
    list_elem(Visited, (End, (_, _))), !.
dijk_solve(Visited, End, Adj, Ans) :-
    findall(
        (NGid, (NWeight, VGid)),
        ( list_elem(Visited, (VGid, (VWeight, _))), list_elem(Adj, (VGid, NGid, Weight)), NWeight is VWeight + Weight ),
        L),
    dijk_minimum(L, VNext),
    VNext = (GidNext, _),
    dijk_removeAdjTo(GidNext, Adj, NextAdj),
    dijk_solve([VNext|Visited], End, NextAdj, Ans).

% dijk_pathFromVisited(1, [(2, (1, 1)),  (3, (1, 1)),  (1, (0, -1))], P).
dijk_pathFromVisited(Gid, Visited, Path) :-
    list_elem(Visited, (Gid, (_, Prev))),
    ((Prev = -1) ->
        Path = [Gid]
    ;   dijk_pathFromVisited(Prev, Visited, Subpath),
        list_append(Subpath, [Gid], Path)
    ).

% dijk_removeAdjTo(3, [(1,2,1), (1,3,1)], Ans ).
dijk_removeAdjTo(_, [], []).
dijk_removeAdjTo(Gid, [(_, Gid, _)|Adjs], Ans) :-
    dijk_removeAdjTo(Gid, Adjs, Ans).
dijk_removeAdjTo(Gid, [(Gid1, Gid2, Weight)|Adjs], [(Gid1, Gid2, Weight)|Ans]) :-
    Gid \= Gid2,
    dijk_removeAdjTo(Gid, Adjs, Ans).

% dijk_minimum([(1, (5, -1)), (2, (3, 1)), (3, (4, 2))], Ans).
dijk_minimum([L], L).
dijk_minimum([(Gid, (Weight, Prev))|Ls], Ans) :-
    dijk_minimum(Ls, (SGid, SWeight, SPrev)),
    (Weight < SWeight ->
        Ans = (Gid, (Weight, Prev))
    ;   Ans = (SGid, (SWeight, SPrev))
    ).
    