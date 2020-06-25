%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% A* ALGORITHM %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

aStar_solve(Visited, End, _, Visited) :-
    list_elem(Visited, (End, _)), !.
aStar_solve(Visited, End, Adj, Ans) :-
    findall(
        (NGid, (Fn, VGid)),
        ( list_elem(Visited, (VGid, _)), list_elem(Adj, (VGid, NGid)),  (gids_dist(VGid, NGid, Gn), gids_dist(NGid, End, Hn), Fn is Gn + Hn) ),
        L),
    aStar_minimum(L, (GidNext, (_, Prev))),
    aStar_removeAdjTo(GidNext, Adj, NextAdj),
    aStar_solve([(GidNext, Prev)|Visited], End, NextAdj, Ans).

aStar_pathFromVisited(Gid, Visited, Path) :-
    list_elem(Visited, (Gid, Prev)),
    ((Prev = -1) ->
        Path = [Gid]
    ;   aStar_pathFromVisited(Prev, Visited, Subpath),
        list_append(Subpath, [Gid], Path)
    ).


aStar_removeAdjTo(_, [], []).
aStar_removeAdjTo(Gid, [(_, Gid)|Adjs], Ans) :-
    aStar_removeAdjTo(Gid, Adjs, Ans).
aStar_removeAdjTo(Gid, [(Gid1, Gid2)|Adjs], [(Gid1, Gid2)|Ans]) :-
    Gid \= Gid2,
    aStar_removeAdjTo(Gid, Adjs, Ans).

aStar_minimum([L], L).
aStar_minimum([(Gid, (HVal, Prev))|Ls], Ans) :-
    aStar_minimum(Ls, (SGid, SHVal, SPrev)),
    (HVal < SHVal ->
        Ans = (Gid, (HVal, Prev))
    ;   Ans = (SGid, (SHVal, SPrev))
    ).