-module(lifeparallel).
-export([life/4]).
-compile([export_all]).

-define(Nprocs, 5).


% Top-level public function

life(Matrix, Nrows, Ncols, Numgen)
        when Nrows >=0, Ncols >=0, Numgen >= 0 ->
    lifemaster:main(Matrix, Numgen, ?Nprocs).



life(Matrix, Nrows, Ncols, Numgen, Nprocs)
        when Nrows >=0, Ncols >=0, Numgen >= 0 ->
    lifemaster:main(Matrix, Numgen, Nprocs).

glider() -> setcells(array2d:new(8,6,0), [{1,0},{2,1},{0,2},{1,2},{2,2}]).

setcells(Matrix, []) -> Matrix;
setcells(Matrix, [{X,Y}|T]) -> setcells(array2d:set(Matrix, X, Y, 1), T).

print(Matrix) -> print(Matrix, 0).
print({_,_,H,_}, H) -> true;
print(M, Y) -> printRow(M, 0, Y), io:fwrite("\n"), print(M, Y+1).
printRow({_,W,_,_}, W, _) -> true;
printRow(M, X, Y) -> io:fwrite(charify(array2d:get(M, X, Y))), printRow(M, X+1, Y).

charify(0) -> ".";
charify(1) -> "O";
charify(-1) -> "X".
