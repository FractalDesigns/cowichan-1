-module(array2d).
-export([new/3, width/1, height/1, get/3, set/4, getRect/5, setRect/4]).


new(Width, Height, InitValue)
        when Width >= 0, Height >= 0 ->
    {array2d, Width, Height,
        array:fix(array:new([{size, Width * Height}, {default, InitValue}]))}.

width({array2d, Width, _, _}) ->
    Width.

height({array2d, _, Height, _}) ->
    Height.

get({array2d, Width, Height, Array}, X, Y)
        when 0 =< X, X < Width, 0 =< Y, Y < Height ->
    array:get(Y * Width + X, Array).

set({array2d, Width, Height, Array}, X, Y, Value)
        when 0 =< X, X < Width, 0 =< Y, Y < Height ->
    {array2d, Width, Height, array:set(Y * Width + X, Value, Array)}.


getRect(Array2D={_, Width, Height, _}, X, Y, W, H)
        when 0 =< X, X + W =< Width, 0 =< Y, Y + H =< Height, W >= 0, H >= 0 ->
    copyRect(Array2D, X, Y, new(W, H, 0), 0, 0, W, H).

setRect(ToArray2D={_, Width, Height, _}, X, Y, FromArray2D={_, W, H, _})
        when 0 =< X, X + W =< Width, 0 =< Y, Y + H =< Height, W >= 0, H >= 0 ->
    copyRect(FromArray2D, 0, 0, ToArray2D, X, Y, W, H).


copyRect(_, _, _, ToArray2D, _, _, _, 0) ->
    ToArray2D;
copyRect(FromArray2D, FromX, FromY, ToArray2D, ToX, ToY, W, H) ->
    Temp = copyRectRow(FromArray2D, FromX, FromY, ToArray2D, ToX, ToY, W),
    copyRect(FromArray2D, FromX, FromY + 1, Temp, ToX, ToY + 1, W, H - 1).

copyRectRow(_, _, _, ToArray2D, _, _, 0) ->
    ToArray2D;
copyRectRow(FromArray2D, FromX, FromY, ToArray2D, ToX, ToY, W) ->
    Temp = set(ToArray2D, ToX, ToY, get(FromArray2D, FromX, FromY)),
    copyRectRow(FromArray2D, FromX + 1, FromY, Temp, ToX + 1, ToY, W - 1).
