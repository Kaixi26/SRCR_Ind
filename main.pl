:-include("adjacencias.pl").
:-include("paragens.pl").
:-include("lists.pl").
:-include("dijkstra.pl").
:-include("a_star.pl").

adjacencia(Carr, GidParag1, GidParag2) :-
    adjacencia(Carr, L),
    list_dropAnyN(L, LL),
    (LL = [GidParag1, GidParag2|_]; LL = [GidParag2, GidParag1|_]).

graph_pathAux(Gid2, Gid2, [Gid2], _).
graph_pathAux(Gid1, Gid2, [Gid1|Path], Adj) :-
    list_filter(filter_adjWithout(Gid1), Adj, AdjNext),
    list_elem(Adj, (Gid1, GidNext)),
    graph_pathAux(GidNext, Gid2, Path, AdjNext).

filter_adjWithout(Gid, (Gid1, Gid2)) :-
    \+ Gid =:= Gid1,
    \+ Gid =:= Gid2.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                  CALCULAR UM TRAJETO ENTRE DOIS GIDs                         %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% DEPTH-FIRST
graph_path(Gid1, Gid2, Path) :-
    findall((X,Y), adjacencia(_, X, Y), Adj),
    graph_pathAux(Gid1, Gid2, Path, Adj).

% DIJKSTRA
graph_pathDijk(Gid1, Gid2, Path) :-
    findall((X, Y, 1), ( adjacencia(_, X, Y), Y \= Gid1 ), Adj),
    dijk_solve([(Gid1, (0, -1))], Gid2, Adj, Ans),
    dijk_pathFromVisited(Gid2, Ans, Path).

% A*
graph_pathAStar(Gid1, Gid2, Path) :-
    findall((X,Y), (adjacencia(_, X, Y), Y \= Gid1), Adj),
    aStar_solve([(Gid1, -1)], Gid2, Adj, Ans),
    aStar_pathFromVisited(Gid2, Ans, Path).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%      CALCULAR UM TRAJETO ENTRE DOIS GIDs USANDO CERTAS OPERADORAS            %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% DEPTH-FIRST
graph_pathOperator(Gid1, Gid2, Operators, Path) :-
    findall((X,Y), adjacencia(_, X, Y), L),
    list_filter(filter_operators(Operators), L, Adj),
    graph_pathAux(Gid1, Gid2, Path, Adj).

% DIJKSTRA
graph_pathOperatorDijk(Gid1, Gid2, Operators, Path) :-
    findall((X, Y, 1), ( adjacencia(_, X, Y), Y \= Gid1 ), L),
    list_filter(filter_operators(Operators), L, Adj),
    dijk_solve([(Gid1, (0, -1))], Gid2, Adj, Ans),
    dijk_pathFromVisited(Gid2, Ans, Path).

% A*
graph_pathOperatorAStar(Gid1, Gid2, Operators, Path) :-
    findall((X,Y), (adjacencia(_, X, Y), Y \= Gid1), L),
    list_filter(filter_operators(Operators), L, Adj),
    aStar_solve([(Gid1, -1)], Gid2, Adj, Ans),
    aStar_pathFromVisited(Gid2, Ans, Path).

filter_operators(Operators, (Gid, _)) :-
    paragem(Gid, (_, (_, _, _, Operator), _, _)),
    list_elem(Operators, Operator).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%      CALCULAR UM TRAJETO ENTRE DOIS GIDs EXCLUINDO CERTAS OPERADORAS         %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% DEPTH-FIRST
graph_pathExcludeOperator(Gid1, Gid2, Operators, Path) :-
    findall((X,Y), adjacencia(_, X, Y), L),
    list_filter(filter_ExcludeOperators(Operators), L, Adj),
    graph_pathAux(Gid1, Gid2, Path, Adj).

% DIJKSTRA
graph_pathExcludeOperatorDijk(Gid1, Gid2, Operators, Path) :-
    findall((X, Y, 1), ( adjacencia(_, X, Y), Y \= Gid1 ), L),
    list_filter(filter_ExcludeOperators(Operators), L, Adj),
    dijk_solve([(Gid1, (0, -1))], Gid2, Adj, Ans),
    dijk_pathFromVisited(Gid2, Ans, Path).

% A*
graph_pathExcludeOperatorAStar(Gid1, Gid2, Operators, Path) :-
    findall((X,Y), (adjacencia(_, X, Y), Y \= Gid1), L),
    list_filter(filter_ExcludeOperators(Operators), L, Adj),
    aStar_solve([(Gid1, -1)], Gid2, Adj, Ans),
    aStar_pathFromVisited(Gid2, Ans, Path).

filter_ExcludeOperators(Operators, (Gid, _)) :-
    paragem(Gid, (_, (_, _, _, Operator), _, _)),
    \+ list_elem(Operators, Operator).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   CALCULAR UM TRAJETO ENTRE DOIS GIDs COM PARAGENS COM/SEM PUBLICIDADE       %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

graph_pathHasAdverts(Gid1, Gid2, HasAdverts, Path) :-
    findall((X,Y), adjacencia(_, X, Y), L),
    list_filter(filter_hasAdverts(HasAdverts), L, Adj),
    graph_pathAux(Gid1, Gid2, Path, Adj).

% DIJKSTRA
graph_pathHasAdvertsDijk(Gid1, Gid2, HasAdverts, Path) :-
    findall((X, Y, 1), ( adjacencia(_, X, Y), Y \= Gid1 ), L),
    list_filter(filter_hasAdverts(HasAdverts), L, Adj),
    dijk_solve([(Gid1, (0, -1))], Gid2, Adj, Ans),
    dijk_pathFromVisited(Gid2, Ans, Path).

% A*
graph_pathHasAdvertsAStar(Gid1, Gid2, HasAdverts, Path) :-
    findall((X,Y), (adjacencia(_, X, Y), Y \= Gid1), L),
    list_filter(filter_hasAdverts(HasAdverts), L, Adj),
    aStar_solve([(Gid1, -1)], Gid2, Adj, Ans),
    aStar_pathFromVisited(Gid2, Ans, Path).

filter_hasAdverts(HasAdverts, (Gid, _)) :-
    paragem(Gid, (_, (_, _, HasAdverts, _), _, _)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%       CALCULAR UM TRAJETO ENTRE DOIS GIDs COM PARAGENS ABRIGADAS             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

graph_pathCovered(Gid1, Gid2, Path) :-
    findall((X,Y), adjacencia(_, X, Y), L),
    list_filter(filter_covered, L, Adj),
    graph_pathAux(Gid1, Gid2, Path, Adj).

% DIJKSTRA
graph_pathCoveredDijk(Gid1, Gid2, Path) :-
    findall((X, Y, 1), ( adjacencia(_, X, Y), Y \= Gid1 ), L),
    list_filter(filter_covered, L, Adj),
    dijk_solve([(Gid1, (0, -1))], Gid2, Adj, Ans),
    dijk_pathFromVisited(Gid2, Ans, Path).

% A*
graph_pathCoveredAStar(Gid1, Gid2, Path) :-
    findall((X,Y), (adjacencia(_, X, Y), Y \= Gid1), L),
    list_filter(filter_covered, L, Adj),
    aStar_solve([(Gid1, -1)], Gid2, Adj, Ans),
    aStar_pathFromVisited(Gid2, Ans, Path).

filter_covered((Gid, _)) :-
    paragem(Gid, (_, (_, Cover, _, _), _, _)),
    Cover \= 'Sem abrigo'.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%             PARAGEM PERTENCENTE AO MAIOR NUMERO DE CARREIRAS                 %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

percurso_maxCarreira([P], (P,Ans)) :-
    paragem_countCarreiras(P, Ans).
percurso_maxCarreira([P|Ps], Ans) :-
    percurso_maxCarreira(Ps, (Subp, Subans)),
    paragem_countCarreiras(P, Pans),
    (Pans > Subans -> (Ans = (P, Pans)); (Ans = (Subp, Subans))).

paragem_countCarreiras(Gid, Ans) :-
    findall(X, (adjacencia(X,Adj), list_elem(Adj, Gid)), L),
    length(L, Ans).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%            MENOR PERCURSO (CRITÉRIO MENOR NÚMERO DE PARAGENS)                %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

graph_leastStopsPath(Gid1, Gid2, Path) :-
    findall((X,Y, 1), adjacencia(_, X, Y), Adj),
    dijk_removeAdjTo(Gid1, Adj, NextAdj),
    dijk_solve([(Gid1, (0, -1))], Gid2, NextAdj, Ans),
    dijk_pathFromVisited(Gid2, Ans, Path).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%              MENOR PERCURSO (CRITÉRIO MENOR DISTÂNCIA)                       %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

graph_leastDistPath(Gid1, Gid2, Path) :-
    findall((X, Y, W), ( adjacencia(_, X, Y), gids_dist(X, Y, W) ), Adj),
    dijk_removeAdjTo(Gid1, Adj, NextAdj),
    dijk_solve([(Gid1, (0, -1))], Gid2, NextAdj, Ans),
    dijk_pathFromVisited(Gid2, Ans, Path).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                  PERCURSO A PASSAR POR VÁRIAS PARAGENS                       %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Dijkstra
graph_stopByDijk([], []).
graph_stopByDijk([Gid], [Gid]).
graph_stopByDijk([Gid1, Gid2|Gids], P) :-
    graph_pathDijk(Gid1, Gid2, LocPath),
    list_init(LocPath, LocPath_),
    graph_stopByDijk([Gid2|Gids], Subpath),
    list_append(LocPath_, Subpath, P).

% A*
graph_stopByAStar([], []).
graph_stopByAStar([Gid], [Gid]).
graph_stopByAStar([Gid1, Gid2|Gids], P) :-
    graph_pathAStar(Gid1, Gid2, LocPath),
    list_init(LocPath, LocPath_),
    graph_stopByAStar([Gid2|Gids], Subpath),
    list_append(LocPath_, Subpath, P).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% UTILITY PREDICATES %%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

path_checkValid([]).
path_checkValid([_]).
path_checkValid([Gid1,Gid2|Gids]) :-
    adjacencia(_, Gid1, Gid2),
    path_checkValid([Gid2|Gids]).

path_dist([], 0).
path_dist([_], 0).
path_dist([X,Y|Ps], Ans) :-
    gids_dist(X, Y, Dist),
    path_dist([Y|Ps], Sub),
    Ans is Dist + Sub.

path_expand([], []).
path_expand([X|Ps], [paragem(X,Y)|Ans]) :-
    paragem(X, Y),
    path_expand(Ps, Ans).

gids_dist(Gid1, Gid2, Ans) :-
    paragem(Gid1, (Coords1, _, _, _)),
    paragem(Gid2, (Coords2, _, _, _)),
    coord_dist(Coords1, Coords2, Ans).

coord_dist((Lat1, Long1), (Lat2, Long2), Ans) :-
    LatSqr is (Lat1 - Lat2)^2
  , LongSqr is (Long1 - Long2)^2
  , Ans is sqrt(LatSqr + LongSqr).

