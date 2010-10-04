-module(mandel).
-define(ITER_LIMIT, 150).
-export([mandel_number/1, mandel_number/2, init_matrix/6]).

% Solving the problem in parallel:
%> X = mandel:init_matrix(20, 20, -1.5, -1.5, 1.5, 1.5). % Matrix with the initial coordinatess
%> Mandelset = multilists:persistent_pmap(fun mandel:mandel_number/1, X).
%> multilists:print(Mandelset).

% Given a point in the complex plane, return the number of iterations it takes
% to diverge, or the iteration limit, whichever is smaller.
mandel_number({X, Y}) -> mandel_number(X, Y, X, Y, 0).
mandel_number(X, Y) -> mandel_number(X, Y, X, Y, 0).

mandel_number(_X0, _Y0, _X, _Y, ?ITER_LIMIT) -> ?ITER_LIMIT;
mandel_number(_X0, _Y0, X, Y, Count) when (X*X + Y*Y) >= 2.0 -> Count;
mandel_number(X0, Y0, X, Y, Count) -> NewX = X*X - Y*Y + Y0,
    NewY = 2*X*Y + X0,
    mandel_number(X0, Y0, NewX, NewY, Count+1).
    
% Create a new Nrows x Ncols matrix with (X0, Y0) as the lower-left corner, 
% and (Dx, Dy) as the upper-right corner.
init_matrix(Nrows, Ncols, X0, Y0, Dx, Dy) -> 
    Xrange = [X0 + A*((Dx-X0)/(Ncols-1)) || A <- lists:seq(0, Ncols-1)],
    Yrange = [Y0 + A*((Dy-Y0)/(Nrows-1)) || A <- lists:seq(0, Nrows-1)],
    init_matrix(Xrange, Yrange).

init_matrix(_Xrange, []) -> [];    
init_matrix(Xrange, [Y|Rem]) -> init_matrix(Xrange, Rem) ++ [lists:zip(Xrange, lists:duplicate(length(Xrange), Y))].


