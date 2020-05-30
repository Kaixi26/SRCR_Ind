:-include("adjacencias.pl").
:-include("paragens.pl").
:-include("lists.pl").

adjacencia(Carr, GidParag1, GidParag2) :-
    adjacencia(Carr, L),
    list_dropAnyN(L, LL),
    (LL = [GidParag1, GidParag2|_]; LL = [GidParag2, GidParag1|_]).

%% CALCULAR UM TRAJETO ENTRE DOIS GIDs
graph_path(Gid1, Gid2, Path) :-
    findall((X,Y), adjacencia(_, X, Y), Adj),
    graph_pathAux(Gid1, Gid2, Path, Adj).

graph_pathAux(Gid2, Gid2, [Gid2], _).
graph_pathAux(Gid1, Gid2, [Gid1|Path], Adj) :-
    list_filter(filter_adjWithout(Gid1), Adj, AdjNext),
    list_elem(Adj, (Gid1, GidNext)),
    graph_pathAux(GidNext, Gid2, Path, AdjNext).

filter_adjWithout(Gid, (Gid1, Gid2)) :-
    \+ Gid =:= Gid1,
    \+ Gid =:= Gid2.
%%

%% CALCULAR UM TRAJETO ENTRE DOIS GIDs com determinadas operadoras
graph_pathOperator(Gid1, Gid2, Operators, Path) :-
    findall((X,Y), adjacencia(_, X, Y), L),
    list_filter(filter_operators(Operators), L, Adj),
    graph_pathAux(Gid1, Gid2, Path, Adj).

filter_operators(Operators, (Gid, _)) :-
    paragem(Gid, (_, (_, _, _, Operator), _, _)),
    list_elem(Operators, Operator).

%%

%% CALCULAR UM TRAJETO ENTRE DOIS GIDs excluindo operadoras
graph_pathExcludeOperator(Gid1, Gid2, Operators, Path) :-
    findall((X,Y), adjacencia(_, X, Y), L),
    list_filter(filter_ExcludeOperators(Operators), L, Adj),
    graph_pathAux(Gid1, Gid2, Path, Adj).

filter_ExcludeOperators(Operators, (Gid, _)) :-
    paragem(Gid, (_, (_, _, _, Operator), _, _)),
    \+ list_elem(Operators, Operator).

%% CALCULAR UM TRAJETO ENTRE DOIS GIDs apenas com/sem publicidade
graph_pathHasAdverts(Gid1, Gid2, HasAdverts, Path) :-
    findall((X,Y), adjacencia(_, X, Y), L),
    list_filter(filter_hasAdverts(HasAdverts), L, Adj),
    graph_pathAux(Gid1, Gid2, Path, Adj).

filter_hasAdverts(HasAdverts, (Gid, _)) :-
    paragem(Gid, (_, (_, _, HasAdverts, _), _, _)).

%%

%% CALCULAR UM TRAJETO ENTRE DOIS GIDs apenas com abrigos 
graph_pathCovered(Gid1, Gid2, Path) :-
    findall((X,Y), adjacencia(_, X, Y), L),
    list_filter(filter_covered, L, Adj),
    graph_pathAux(Gid1, Gid2, Path, Adj).

filter_covered((Gid, _)) :-
    paragem(Gid, (_, (_, Cover, _, _), _, _)),
    Cover \= 'Sem abrigo'.

%%

%% Identificar a paragem com mais carreiras num percurso

percurso_maxCarreira([P], (P,Ans)) :-
    paragem_countCarreiras(P, Ans).
percurso_maxCarreira([P|Ps], Ans) :-
    percurso_maxCarreira(Ps, (Subp, Subans)),
    paragem_countCarreiras(P, Pans),
    (Pans > Subans -> (Ans = (P, Pans)); (Ans = (Subp, Subans))).

paragem_countCarreiras(Gid, Ans) :-
    findall(X, (adjacencia(X,Adj), list_elem(Adj, Gid)), L),
    length(L, Ans).

%%

shortestPath_setupDijkstra(StartGid, Q) :-
    findall((shortestPath_vertex(Gid,(Dist, Prev))), (Dist is inf, Prev is -2, paragem(Gid, _)), L),
    list_mapSideEffect(shortestPath_updateVertex, L),
    shortestPath_updateVertex(shortestPath_vertex(StartGid, (0, -1))).

shortestPath_updateVertex(shortestPath_vertex(Gid, (Dist, Prev))) :-
    findall(shortestPath_vertex(Gid_,(Dist_, Prev_)), shortestPath_vertex(Gid,(Dist_, Prev_)), L),
    list_mapSideEffect(retract, L),
    assert(shortestPath_vertex(Gid, (Dist, Prev))).






path_dist([_], 0).
path_dist([P1,P2|Xs], Ans) :-
    path_dist([P2|Xs], Subans)
  , parag_dist(P1, P2, Dist)
  , Ans is Subans + Dist.

parag_dist(paragem(_,(Coords1, _, _, _)), paragem(_,(Coords2, _, _, _)), Ans) :-
    coord_dist(Coords1, Coords2, Ans).

coord_dist((Lat1, Long1), (Lat2, Long2), Ans) :-
    LatSqr is (Lat1 - Lat2)^2
  , LongSqr is (Long1 - Long2)^2
  , Ans is sqrt(LatSqr + LongSqr).

