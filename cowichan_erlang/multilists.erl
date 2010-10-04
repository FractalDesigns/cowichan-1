-module(multilists).
-export([zeroes/1, get/2, set/3, print/1, pmap/2, persistent_pmap/2]).

% Multi-d arrays implemented using nested lists (indexing is from 1 right now, but should probably be changed)

% Array initialized to zeroes. Takes a list corresponding to the size of
% dimensions. Zeroes([100, 10, 10]) ~= new int[100][10][10]
zeroes([Dim|[]]) -> lists:duplicate(Dim, 0);
zeroes([Dim|Rem]) -> lists:duplicate(Dim, zeroes(Rem)).

% get([5, 10], arr) ~= arr[5][10]
get([Coord|[]], Arr) -> lists:nth(Coord, Arr);
get([Coord|Rest], Arr) -> get(Rest, lists:nth(Coord, Arr)).

% set([5, 10], arr, 17) ~= arr[5][10] = 17
set([Coord|[]], Arr, Value) -> lists:sublist(Arr, Coord-1) ++ [Value] ++ lists:sublist(Arr, Coord+1, length(Arr));
set([Coord|Rest], Arr, Value) -> lists:sublist(Arr, Coord-1) ++ [set(Rest, get([Coord], Arr), Value)] ++ lists:sublist(Arr, Coord+1, length(Arr)).

list_to_str([]) -> "~n";
list_to_str([X|Rem]) when is_integer(X) -> integer_to_list(X) ++ " " ++ list_to_str(Rem);
list_to_str([X|Rem]) when is_float(X) -> io_lib:format("~.3f", [X]) ++ " " ++ list_to_str(Rem);
list_to_str([{X,Y}|Rem]) when is_integer(X) -> "(" ++ integer_to_list(X) ++ ", " ++ integer_to_list(Y) ++ ")" ++ list_to_str(Rem);
list_to_str([{X,Y}|Rem]) when is_float(X) -> "(" ++ io_lib:format("~.3f", [X]) ++ ", " ++ io_lib:format("~.3f", [Y]) ++ ")" ++ list_to_str(Rem);
list_to_str([X|Rem]) -> list_to_str(X) ++ list_to_str(Rem).

print(Arr) -> io:format(list_to_str(Arr)).

% parallel 2-d map
pmap(Fun, Arr) -> Parent = self(),
    Pids = lists:map(fun(Ele) -> spawn(fun() -> pmap_f(Parent, Fun, Ele) end) end, Arr),
    pmap_gather(Pids).
    
pmap_f(Parent, Fun, Element) -> Parent ! {self(), lists:map(Fun, Element)}.

pmap_gather([H|T]) ->
    receive
        {H, Ret} -> [Ret|pmap_gather(T)]
    end;
pmap_gather([]) -> [].
    
% parallel 2-d map with process restarting
persistent_pmap(Fun, Arr) -> Parent = self(),
    Pids = lists:map(fun(Ele) -> spawn(fun() -> pmap_f(Parent, Fun, Ele) end) end, Arr),
    persistent_gather(lists:zip(Pids, Arr), Fun).
    
persistent_gather([], _) -> [];
persistent_gather([{Pid, Elem}|T], Fun) ->
    receive
        {Pid, Ret} -> [Ret|persistent_gather(T, Fun)]
    after 2000 -> 
        spawn(fun() -> pmap_f(self(), Fun, Elem) end),
        persistent_gather([{Pid, Elem}|T], Fun)
    end.
    
