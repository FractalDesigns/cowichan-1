-module(lifeworker).
-export([main/4, blankMain/4]).
-compile([export_all]).


% Blank worker node, always sending a column of zeros

blankMain(_, 0, _, _) ->
    true;
blankMain(Height, Numgen, Neighbor, Type) ->
    blankFlush(),
    Neighbor ! {Type, array2d:new(1, Height, 0)},
    blankMain(Height, Numgen - 1, Neighbor, Type).

blankFlush() ->
    receive
        {leftEdge , _} -> blankFlush();
        {rightEdge, _} -> blankFlush()
    after
        0 -> true
    end.


% Worker function

main(MasterPid, ProcIndex, Matrix, Numgen) ->
    Neighs = getNeighbors(),
    PaddedMatrix = padMatrix(Matrix),
    NewMatrix = iterate(Neighs, PaddedMatrix, Numgen),
    MasterPid ! {done, ProcIndex, unpadMatrix(NewMatrix)}.


padMatrix(Matrix) ->
    Width = array2d:width(Matrix),
    Height = array2d:height(Matrix),
    NewMatrix = array2d:new(Width + 2, Height + 2, 0),
    array2d:setRect(NewMatrix, 1, 1, Matrix).

unpadMatrix(Matrix) ->
    Width = array2d:width(Matrix),
    Height = array2d:height(Matrix),
    array2d:getRect(Matrix, 1, 1, Width - 2, Height - 2).


getNeighbors() ->
    receive
        {neighbors, LeftNeigh, RightNeigh} -> {LeftNeigh, RightNeigh}
    end.

receiveEdges(Matrix) ->
    receive
        {leftEdge, LeftEdge} ->
            Temp = array2d:setRect(Matrix, 0, 1, LeftEdge),
            receive {rightEdge, RightEdge} ->
                array2d:setRect(Temp, array2d:width(Matrix) - 1, 1, RightEdge)
            end
    end.


iterate(_, Matrix, 0) ->
    Matrix;
iterate(Neighs={LeftNeigh, RightNeigh}, Matrix, Numgen) ->
    LeftNeigh ! {rightEdge, array2d:getRect(Matrix, 1, 1, 1, array2d:height(Matrix) - 2)},
    RightNeigh ! {leftEdge, array2d:getRect(Matrix, array2d:width(Matrix) - 2, 1, 1, array2d:height(Matrix) - 2)},
    TempMatrix = receiveEdges(Matrix),
    NewMatrix = compute(TempMatrix),
    iterate(Neighs, NewMatrix, Numgen - 1).


compute(Matrix) ->
    mapArray2D(Matrix,
        fun(Mat, W, H, X, Y) when X =:= 0; X =:= W - 1; Y =:= 0; Y =:= H - 1
               -> array2d:get(Mat, X, Y);
           (Mat, _, _, X, Y)
               -> Self = array2d:get(Mat, X, Y),
                  Neighs = array2d:get(Mat, X - 1, Y - 1)
                         + array2d:get(Mat, X - 1, Y + 0)
                         + array2d:get(Mat, X - 1, Y + 1)
                         + array2d:get(Mat, X + 0, Y - 1)
                         + array2d:get(Mat, X + 0, Y + 1)
                         + array2d:get(Mat, X + 1, Y - 1)
                         + array2d:get(Mat, X + 1, Y + 0)
                         + array2d:get(Mat, X + 1, Y + 1),
                  lifeOrDeath(Self, Neighs)
        end
    ).


lifeOrDeath(0, 3) -> 1;
lifeOrDeath(0, _) -> 0;
lifeOrDeath(1, 2) -> 1;
lifeOrDeath(1, 3) -> 1;
lifeOrDeath(1, _) -> 0.


mapArray2D(Matrix, Fun) ->
    Width = array2d:width(Matrix),
    Height = array2d:height(Matrix),
    mapArray2D(Matrix, array2d:new(Width, Height, 0), Width, Height, Fun, 0).

mapArray2D(_, NewMatrix, _, Height, _, Height) ->
    NewMatrix;
mapArray2D(OldMatrix, NewMatrix, Width, Height, Fun, Y) ->
    Temp = mapArray2DRow(OldMatrix, NewMatrix, Width, Height, Fun, 0, Y),
    mapArray2D(OldMatrix, Temp, Width, Height, Fun, Y + 1).

mapArray2DRow(_, NewMatrix, Width, _, _, Width, _) ->
    NewMatrix;
mapArray2DRow(OldMatrix, NewMatrix, Width, Height, Fun, X, Y) ->
    Temp = array2d:set(NewMatrix, X, Y, Fun(OldMatrix, Width, Height, X, Y)),
    mapArray2DRow(OldMatrix, Temp, Width, Height, Fun, X + 1, Y).
