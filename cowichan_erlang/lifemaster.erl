-module(lifemaster).
-export([main/3]).


% Master function (spawns workers, hands out work, joins them, merges results)

main(Matrix, Numgen, Nprocs) ->
    Width = array2d:width(Matrix),
    Height = array2d:height(Matrix),
    Stride = (Width + Nprocs - 1) div Nprocs,
    WorkerPids = mySpawn(Matrix, Stride, Numgen, 0, Nprocs, []),
    LeftBlankPid = spawn(lifeworker, blankMain, [Height, Numgen, lists:nth(1, WorkerPids), leftEdge]),
    RightBlankPid = spawn(lifeworker, blankMain, [Height, Numgen, lists:last(WorkerPids), rightEdge]),
    AllPids = [LeftBlankPid] ++ WorkerPids ++ [RightBlankPid],
    forEachIndex(WorkerPids,
        fun(L, I) ->
            lists:nth(I, L) ! {neighbors, lists:nth(I, AllPids), lists:nth(I + 2, AllPids)}
        end
    ),
    myJoin(Nprocs, Stride, array2d:new(Width, Height, -1)).

mySpawn(_, _, _, Nprocs, Nprocs, Pids) ->
    lists:reverse(Pids);
mySpawn(Matrix, Stride, Numgen, ProcIndex, Nprocs, Pids) ->
    Width = array2d:width(Matrix),
    Height = array2d:height(Matrix),
    Offset = min(ProcIndex * Stride, Width),
    Slice = array2d:getRect(Matrix, Offset, 0, min(Stride, Width - Offset), Height),
    NewPids = [spawn(lifeworker, main, [self(), ProcIndex, Slice, Numgen]) | Pids],
    mySpawn(Matrix, Stride, Numgen, ProcIndex + 1, Nprocs, NewPids).

myJoin(0, _, Matrix) ->
    Matrix;
myJoin(Nprocs, Stride, Matrix) ->
    receive
        {done, ProcIndex, Mat} ->
            Offset = min(ProcIndex * Stride, array2d:width(Matrix)),
            NewMatrix = array2d:setRect(Matrix, Offset, 0, Mat),
            myJoin(Nprocs - 1, Stride, NewMatrix)
    end.

forEachIndex(List, Fun) ->
    forEachIndex(List, List, Fun, 1).

forEachIndex(_, [], _, _) ->
    true;
forEachIndex(List, [_|T], Fun, I) ->
    Fun(List, I),
    forEachIndex(List, T, Fun, I + 1).
