/* ----------------------------------------------------------
    CSC384 Assignment 2 file

% Surname: Borzenko
% First Name: Andrew
% Student Number: 993875339

  ------------------------------------------------------ */

%do not chagne the follwoing line!
:- ensure_loaded('play.pl').

% /* ------------------------------------------------------ */
%               IMPORTANT! PLEASE READ THIS SUMMARY:
%       This files gives you some useful helpers (set &get).
%       Your job is to implement several predicates using
%       these helpers (feel free to add your own helpers if needed,
%       MAKE SURE to write comments for all your helpers, marks will
%       be deducted for bad style!).
%
%       Implement the following predicates at their designated space
%       in this file (we suggest to have a look at file ttt.pl  to
%       see how the implementations is done for game tic-tac-toe.
%
%          * initialize(InitialState,InitialPlyr).
%          * winner(State,Plyr) 
%          * tie(State)
%          * terminal(State) 
%          * moves(Plyr,State,MvList)
%          * nextState(Plyr,Move,State,NewState,NextPlyr)
%          * validmove(Plyr,State,Proposed)
%          * h(State,Val)  (see question 2 in the handout)
%          * lowerBound(B)
%          * upperBound(B)
% /* ------------------------------------------------------ */







% /* ------------------------------------------------------ */

% We use the following State Representation: 
% [Row0, Row1 ... Rown] (ours is 6x6 so n = 5 ).
% each Rowi is a LIST of 6 elements '.' or '1' or '2' as follows: 
%    . means the position is  empty
%    1 means player one has a stone in this position
%    2 means player two has a stone in this position. 



% given helper: Inital state of the board 
initBoard([ [.,.,.,.,.,.], 
            [.,.,.,.,.,.],
	    [.,.,1,2,.,.], 
	    [.,.,2,1,.,.], 
            [.,.,.,.,.,.], 
	    [.,.,.,.,.,.] ]).
 
%%%%%%%%%%%%%%%%%% IMPLEMENT: initialize(...)%%%%%%%%%%%%%%%%%%%%%
%%% Using initBoard define initialize(InitialState,InitialPlyr). 
%%%  holds iff InitialState is the initial state and 
%%%  InitialPlyr is the player who moves first. 

initialize(InitialState,InitialPlyr) :-
    initBoard(InitialState),
    InitialPlyr is 1.


% ensure_loaded('othello.pl').
% initialize(B,P).
% next_xy([[1,2,1,2,1,2],[1,2,1,2,1,2],[1,2,1,2,1,2],[1,2,2,1,1,2],[1,2,1,2,1,2],[1,2,1,2,1,2]], [1,2], [NX,NY]).
% next_xy([[1,2,1,2,1,2],[1,2,1,2,1,2],[1,2,1,2,1,2],[1,2,2,1,1,2],[1,2,1,2,1,2],[1,2,1,2,1,2]], [1,5], [NX,NY]).
% next_xy([[1,2,1,2,1,2],[1,2,1,2,1,2],[1,2,1,2,1,2],[1,2,2,1,1,2],[1,2,1,2,1,2],[1,2,1,2,1,2]], [5,5], [NX,NY]).
% terminal([[1,2,1,2,1,2],[1,2,1,2,1,2],[1,2,1,2,1,2],[1,2,2,1,1,2],[1,2,1,2,1,2],[1,2,1,2,1,2]]).
% number_of_V_update_count(1,1,2,NewCount).
% number_of_V_update_count(1,2,2,NewCount).
% number_of_V([[1,2,1,2,1,2],[1,2,1,.,.,2],[1,2,1,2,1,2],[1,2,2,.,1,2],[1,2,1,.,1,2],[1,2,1,2,1,2]],1,N).
% number_of_V([[1,2,1,2,1,2],[1,2,1,.,.,2],[1,2,1,2,1,2],[1,2,2,.,1,2],[1,2,1,.,1,2],[1,2,1,2,1,2]],2,N).
% number_of_V([[1,2,1,2,1,2],[1,2,1,.,.,2],[1,2,1,2,1,2],[1,2,2,.,1,2],[1,2,1,.,1,2],[1,2,1,2,1,2]],.,N).
% winner([[1,2,1,2,1,2],[1,2,1,2,1,2],[1,2,1,2,1,2],[1,2,2,1,1,2],[1,2,1,2,1,2],[1,2,1,2,1,2]],P).
% tie([[1,2,1,2,1,2],[1,2,1,2,1,2],[1,2,1,2,1,2],[1,2,2,1,1,2],[1,2,1,2,1,2],[1,2,1,2,1,2]]).
% winner([[1,2,1,2,1,2],[1,2,1,2,1,2],[1,2,1,2,1,2],[1,2,2,1,1,2],[1,2,1,2,1,2],[1,2,1,2,1,1]],P).
% validmove(1,[[.,.,.,.,.,.],[.,.,.,.,.,.],[.,.,1,2,.,.],[.,.,2,1,.,.],[.,.,.,.,.,.],[.,.,.,.,.,.]],[3,1]).
% validmove(1,[[.,.,.,.,.,.],[.,.,.,.,.,.],[.,.,1,2,.,.],[.,.,2,1,.,.],[.,.,.,.,.,.],[.,.,.,.,.,.]],[2,1]).
% moves(1,[[.,.,.,.,.,.],[.,.,.,.,.,.],[.,.,1,2,.,.],[.,.,2,1,.,.],[.,.,.,.,.,.],[.,.,.,.,.,.]],MvList).
% nextState(1,[3,1],[[.,.,.,.,.,.],[.,.,.,.,.,.],[.,.,1,2,.,.],[.,.,2,1,.,.],[.,.,.,.,.,.],[.,.,.,.,.,.]],NewState,NextPlyr).
% nextState(1,[3,1],[[.,.,.,.,.,.],[.,.,.,.,.,.],[.,.,1,2,.,.],[.,.,2,1,.,.],[.,.,.,.,.,.],[.,.,.,.,.,.]],NewState,NextPlyr),showState(NewState),nextState(NextPlyr,[4,3],NewState,NewState2,NextPlyr2),showState(NewState2).
% validmove(1,[[.,.,.,.,.,.],[.,.,.,1,.,.],[.,.,1,1,.,.],[.,.,2,2,2,.],[.,.,.,.,.,.],[.,.,.,.,.,.]],[5,4]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%winner(...)%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% define winner(State,Plyr) here.  
%     - returns winning player if State is a terminal position and
%     Plyr has a higher score than the other player 

winner(State,Plyr) :-
    terminal(State),
    number_of_V(State,1,N1),
    number_of_V(State,2,N2),
    winner_helper(N1,N2,Plyr).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%winner_helper(...)%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% define winner_helper(N1,N2,Plyr) here.
%     - returns winning player based on number of ones and twos on the
%       board.

winner_helper(N1,N2,1) :-
    N1 < N2.

winner_helper(N1,N2,2) :-
    N2 < N1.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%number_of_V(...)%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% define number_of_V(State,V,N) here.
%     - returns number of values V on the board.

number_of_V(State,V,N) :-
    number_of_V_helper(State,[0,0],V,0,N).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%number_of_V_helper(...)%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% define number_of_V_helper(State,[X,Y],V,Count,N) here.
%     - checks if value at [X,Y] is V and updates count, then calls
%       itself recursively with next [X,Y].

number_of_V_helper(_,[-1,-1],_,Count,Count).

number_of_V_helper(State,[X,Y],V,Count,N) :-
    X >= 0,
    Y >= 0,
    get(State,[X,Y],AV),
    number_of_V_update_count(AV,V,Count,NewCount),
    next_xy(State,[X,Y],[NextX,NextY]),
    number_of_V_helper(State,[NextX,NextY],V,NewCount,N).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%number_of_V_update_count(...)%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% define number_of_V_update_count(AV,V,Count,NewCount) here.
%     - increment count if V == AV.

number_of_V_update_count(AV,V,Count,NewCount) :-
    AV == V,
    NewCount is Count + 1.

number_of_V_update_count(AV,V,Count,NewCount) :-
    not(AV == V),
    NewCount is Count.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%tie(...)%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% define tie(State) here. 
%    - true if terminal State is a "tie" (no winner) 

tie(State) :-
    terminal(State),
    number_of_V(State,1,N1),
    number_of_V(State,2,N2),
    N1 == N2.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%terminal(...)%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% define terminal(State). 
%   - true if State is a terminal   

terminal(State) :-
    terminal_helper(State,[0,0]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%terminal_helper(...)%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% define terminal_helper(State,[X,Y]). 
%   - checks if value at [X,Y] can be in terminal state, then calls
%     itself recursively with next [X,Y].

terminal_helper(_,[-1,-1]).

terminal_helper(State,[X,Y]) :-
    X >= 0,
    Y >= 0,
    get(State,[X,Y],Value),
    not(Value == '.'),
    next_xy(State,[X,Y],[NextX,NextY]),
    terminal_helper(State,[NextX,NextY]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%next_xy(...)%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% define next_xy(State,[X,Y],[NextX,NextY]). 
%   - return next [X,Y] coordinates in top to bottom, then left to
%     right order. [-1,-1] is returned if no next [X,Y] is available.

next_xy(State,[X,Y],[-1,-1]) :-
    length(State, L),
    Y >= L - 1,
    X >= L - 1.

next_xy(State,[X,Y],[NextX,NextY]) :-
    length(State, L),
    Y < L - 1,
    NextX is X,
    NextY is Y + 1.

next_xy(State,[X,Y],[NextX,NextY]) :-
    length(State, L),
    Y >= L - 1,
    X < L - 1,
    NextX is X + 1,
    NextY is 0.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%showState(State)%%%%%%%%%%%%%%%%%%%%%%%%%%
%% given helper. DO NOT  change this. It's used by play.pl
%% 
showState( G ) :- 
	printRows( G ). 
 
printRows( [] ). 
printRows( [H|L] ) :- 
	printList(H),
	nl,
	printRows(L). 

printList([]).
printList([H | L]) :-
	write(H),
	write(' '),
	printList(L).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%moves(...)%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 
%% define moves(Plyr,State,MvList). 
%   - returns list MvList of all legal moves Plyr can make in State
%

moves(Plyr,State,MvList) :-
    moves_helper(Plyr,State,[0,0],[],List),
    moves_add_n(List,MvList).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%moves_add_n(...)%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 
%% define moves_add_n(List,MvList). 
%   - adds 'n' (pass move) to MvList if List is empty
%

moves_add_n([],[n]).

moves_add_n(List,List) :-
    not(List == []).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%moves_helper(...)%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 
%% define moves_helper(Plyr,State,[X,Y],List,MvList). 
%   - adds move [X,Y] to List if it is legal, then calls itself
%     recursively with next [X,Y].
%

moves_helper(_,_,[-1,-1],MvList,MvList).

moves_helper(Plyr,State,[X,Y],List,MvList) :-
    X >= 0,
    Y >= 0,
    moves_update_list(Plyr,State,[X,Y],List,NewList),
    next_xy(State,[X,Y],[NextX,NextY]),
    moves_helper(Plyr,State,[NextX,NextY],NewList,MvList).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%moves_update_list(...)%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 
%% define moves_update_list(Plyr,State,[X,Y],List,NewList). 
%   - adds move [X,Y] to List if it is legal.
%

moves_update_list(Plyr,State,[X,Y],List,List) :-
    not(validmove(Plyr,State,[X,Y])).

moves_update_list(Plyr,State,[X,Y],List,NewList) :-
    validmove(Plyr,State,[X,Y]),
    append(List,[[X,Y]],NewList).


%%%%%%%%%%%%%%nextState(...)%%%%%%%%%%%%%%%%%%%%
%% 
%% define nextState(Plyr,Move,State,NewState,NextPlyr). 
%   - given that Plyr makes Move in State, it determines NewState (i.e. the next 
%     state) and NextPlayer (i.e. the next player who will move).
%

nextState(Plyr,Move,State,NewState,NextPlyr) :-
    set(State,State1,Move,Plyr),
    flip_cells(Plyr,State1,Move,[-1,-1],State2),
    flip_cells(Plyr,State2,Move,[0,-1],State3),
    flip_cells(Plyr,State3,Move,[1,-1],State4),
    flip_cells(Plyr,State4,Move,[1,0],State5),
    flip_cells(Plyr,State5,Move,[1,1],State6),
    flip_cells(Plyr,State6,Move,[0,1],State7),
    flip_cells(Plyr,State7,Move,[-1,1],State8),
    flip_cells(Plyr,State8,Move,[-1,0],NewState),
    next_player(Plyr,NewState,NextPlyr).


%%%%%%%%%%%%%%next_player(...)%%%%%%%%%%%%%%%%%%%%
%% 
%% define next_player(Plyr,NewState,NextPlyr). 
%   - returns NextPlyr after Plyr generates NewState depending on
%     whether OtherPlyr has any legal moves.

next_player(Plyr,NewState,Plyr) :-
    other_player(Plyr,OtherPlyr),
    moves(OtherPlyr,NewState,MvList),
    MvList == [n].

next_player(Plyr,NewState,OtherPlyr) :-
    other_player(Plyr,OtherPlyr),
    moves(OtherPlyr,NewState,MvList),
    not(MvList == [n]).


%%%%%%%%%%%%%%flip_cells(...)%%%%%%%%%%%%%%%%%%%%
%% 
%% define flip_cells(Plyr,State,Move,Direction,NewState). 
%   - flips cells along Direction if necessary.
%

flip_cells(Plyr,State,Move,Direction,State) :-
    find_matching_xy(Plyr,State,Move,Direction,Matching),
    Matching == [-1,-1].

flip_cells(Plyr,State,Move,Direction,NewState) :-
    find_matching_xy(Plyr,State,Move,Direction,Matching),
    not(Matching == [-1,-1]),
    flip_cells_helper(State,Move,Direction,Matching,NewState).


%%%%%%%%%%%%%%flip_cells_helper(...)%%%%%%%%%%%%%%%%%%%%
%% 
%% define flip_cells_helper(State,Move,Direction,Matching,NewState). 
%   - flips cells C where Move < C < Matching.
%

flip_cells_helper(State,[X,Y],[DX,DY],[MX,MY],State) :-
    CX is X + DX,
    CY is Y + DY,
    [CX,CY] == [MX,MY].

flip_cells_helper(State,[X,Y],[DX,DY],[MX,MY],NewState) :-
    CX is X + DX,
    CY is Y + DY,
    not([CX,CY] == [MX,MY]),
    get(State,[CX,CY],Plyr),
    other_player(Plyr,OtherPlyr),
    set(State,NextState,[CX,CY],OtherPlyr),
    flip_cells_helper(NextState,[CX,CY],[DX,DY],[MX,MY],NewState).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%validmove(...)%%%%%%%%%%%%%%%%%%%%
%% 
%% define validmove(Plyr,State,Proposed). 
%   - true if Proposed move by Plyr is valid at State.

validmove(Plyr,State,[X,Y]) :-
    inbounds(State,[X,Y]),
    get(State,[X,Y],Value),
    Value == '.',
    find_matching_xy(Plyr,State,[X,Y],[-1,-1],[XLU,YLU]),
    find_matching_xy(Plyr,State,[X,Y],[0,-1],[XU,YU]),
    find_matching_xy(Plyr,State,[X,Y],[1,-1],[XRU,YRU]),
    find_matching_xy(Plyr,State,[X,Y],[1,0],[XR,YR]),
    find_matching_xy(Plyr,State,[X,Y],[1,1],[XRD,YRD]),
    find_matching_xy(Plyr,State,[X,Y],[0,1],[XD,YD]),
    find_matching_xy(Plyr,State,[X,Y],[-1,1],[XLD,YLD]),
    find_matching_xy(Plyr,State,[X,Y],[-1,0],[XL,YL]),
    append([[XLU,YLU]],[[XU,YU]],R1),
    append(R1,[[XRU,YRU]],R2),
    append(R2,[[XR,YR]],R3),
    append(R3,[[XRD,YRD]],R4),
    append(R4,[[XD,YD]],R5),
    append(R5,[[XLD,YLD]],R6),
    append(R6,[[XL,YL]],Result),
    not(Result == [[-1,-1],[-1,-1],[-1,-1],[-1,-1],[-1,-1],[-1,-1],[-1,-1],[-1,-1]]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%inbounds(...)%%%%%%%%%%%%%%%%%%%%
%% 
%% define inbounds(State,[X,Y]).
%   - true if [X,Y] is in bounds of State.

inbounds(State,[X,Y]) :-
    X >= 0,
    Y >= 0,
    length(State,L),
    X =< L - 1,
    Y =< L - 1.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%other_player(...)%%%%%%%%%%%%%%%%%%%%
%% 
%% define other_player(Plyr,OtherPlyr).
%   - return other player.

other_player(1,2).

other_player(2,1).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%find_matching_xy(...)%%%%%%%%%%%%%%%%%%%%
%% 
%% define find_matching_xy(Plyr,State,Move,Direction,Matching). 
%   - searches from Move [X,Y] in the Direction [DX,DY] to return
%     Matching [MX,MY]. Cell [MX,MY] must belong to Plyr
%     and all cells inbetween (there must be at least one)
%     must belong to the other player.
%     [-1,-1] is returned if there is no matching cell.

find_matching_xy(_,State,[X,Y],[DX,DY],[-1,-1]) :-
    NX is X + DX,
    NY is Y + DY,
    not(inbounds(State,[NX,NY])).

find_matching_xy(Plyr,State,[X,Y],[DX,DY],[-1,-1]) :-
    NX is X + DX,
    NY is Y + DY,
    inbounds(State,[NX,NY]),
    get(State,[NX,NY],Value),
    other_player(Plyr,OtherPlyr),
    not(Value == OtherPlyr).

find_matching_xy(Plyr,State,[X,Y],[DX,DY],[MX,MY]) :-
    NX is X + DX,
    NY is Y + DY,
    inbounds(State,[NX,NY]),
    get(State,[NX,NY],Value),
    other_player(Plyr,OtherPlyr),
    Value == OtherPlyr,
    CX is NX + DX,
    CY is NY + DY,
    find_matching_xy_helper(Plyr,State,[CX,CY],[DX,DY],[MX,MY]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%find_matching_xy_helper(...)%%%%%%%%%%%%%%%%%%%%
%% 
%% define find_matching_xy_helper(Plyr,State,Current,Direction,Matching). 
%   - returns current cell if it contains Plyr.
%     if it contains OtherPlyr then recursively calls itself
%     with next [CX,CY].
%     [-1,-1] is returned if there is no matching cell.

find_matching_xy_helper(_,State,[CX,CY],_,[-1,-1]) :-
    not(inbounds(State,[CX,CY])).

find_matching_xy_helper(_,State,[CX,CY],_,[-1,-1]) :-
    inbounds(State,[CX,CY]),
    get(State,[CX,CY],Value),
    Value == '.'.

find_matching_xy_helper(Plyr,State,[CX,CY],_,[CX,CY]) :-
    inbounds(State,[CX,CY]),
    get(State,[CX,CY],Value),
    Value == Plyr.

find_matching_xy_helper(Plyr,State,[CX,CY],[DX,DY],[MX,MY]) :-
    inbounds(State,[CX,CY]),
    get(State,[CX,CY],Value),
    other_player(Plyr,OtherPlyr),
    Value == OtherPlyr,
    NX is CX + DX,
    NY is CY + DY,
    find_matching_xy_helper(Plyr,State,[NX,NY],[DX,DY],[MX,MY]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%h(State,Val)%%%%%%%%%%%%%%%%%%%%%%%%%
%% 
%% define h(State,Val). 
%   - given State, returns heuristic Val of that state
%   - larger values are good for Max, smaller values are good for Min
%   NOTE1. If State is terminal h should return its true value.
%   NOTE2. If State is not terminal h should be an estimate of
%          the value of state (see handout on ideas about
%          good heuristics.

h(State,Val) :-
    not(terminal(State)),
    number_of_V(State,1,N1),
    number_of_V(State,2,N2),
    Val is N2 - N1.

h(State,Val) :-
    terminal(State),
    number_of_V(State,1,N1),
    number_of_V(State,2,N2),
    Val is N2 - N1.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%lowerBound(B)%%%%%%%%%%%%%%%%%%%%%%%%%
%% 
%% define lowerBound(B).  
%   - returns a value B that is less than the actual or heuristic value
%     of all states.

lowerBound(-37).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%upperBound(B)%%%%%%%%%%%%%%%%%%%%%%%%%
%% 
%% define upperBound(B). 
%   - returns a value B that is greater than the actual or heuristic value
%     of all states.

upperBound(37).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                       %
%                                                                       %
%                Given   UTILITIES                                      %
%                   do NOT change these!                                %
%                                                                       %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% get(Board, Point, Element)
%    : get the contents of the board at position column X and row Y
% set(Board, NewBoard, [X, Y], Value):
%    : set Value at column X row Y in Board and bind resulting grid to NewBoard
%
% The origin of the board is in the upper left corner with an index of
% [0,0], the upper right hand corner has index [5,0], the lower left
% hand corner has index [0,5], the lower right hand corner has index
% [5,5] (on a 6x6 board).
%
% Example
% ?- initBoard(B), showState(B), get(B, [2,3], Value). 
%. . . . . . 
%. . . . . . 
%. . 1 2 . . 
%. . 2 1 . . 
%. . . . . . 
%. . . . . . 
%
%B = [['.', '.', '.', '.', '.', '.'], ['.', '.', '.', '.', '.', '.'], 
%     ['.', '.', 1, 2, '.', '.'], ['.', '.', 2, 1, '.'|...], 
%     ['.', '.', '.', '.'|...], ['.', '.', '.'|...]]
%Value = 2 
%Yes
%?- 
%
% Setting values on the board
% ?- initBoard(B),  showState(B),set(B, NB1, [2,4], 1), set(NB1, NB2, [2,3], 1),  showState(NB2). 
%
% . . . . . . 
% . . . . . . 
% . . 1 2 . . 
% . . 2 1 . . 
% . . . . . . 
% . . . . . .
% 
% . . . . . . 
% . . . . . . 
% . . 1 2 . . 
% . . 1 1 . . 
% . . 1 . . . 
% . . . . . .
%
%B = [['.', '.', '.', '.', '.', '.'], ['.', '.', '.', '.', '.', '.'], ['.', '.', 
%1, 2, '.', '.'], ['.', '.', 2, 1, '.'|...], ['.', '.', '.', '.'|...], ['.', '.',
% '.'|...]]
%NB1 = [['.', '.', '.', '.', '.', '.'], ['.', '.', '.', '.', '.', '.'], ['.', '.'
%, 1, 2, '.', '.'], ['.', '.', 2, 1, '.'|...], ['.', '.', 1, '.'|...], ['.', '.
%', '.'|...]]
%NB2 = [['.', '.', '.', '.', '.', '.'], ['.', '.', '.', '.', '.', '.'], ['.', '.'
%, 1, 2, '.', '.'], ['.', '.', 1, 1, '.'|...], ['.', '.', 1, '.'|...], ['.', 
%'.', '.'|...]]

% get(Board, Point, Element): get the value of the board at position
% column X and row Y (indexing starts at 0).
get( Board, [X, Y], Value) :- 
	nth0( Y, Board, ListY), 
	nth0( X, ListY, Value).
 
% set( Board, NewBoard, [X, Y], Value) 

set( [Row|RestRows], [NewRow|RestRows], [X, 0], Value)
    :- setInList(Row, NewRow, X, Value). 

set( [Row|RestRows], [Row|NewRestRows], [X, Y], Value) :- 
	Y > 0, 
	Y1 is Y-1, 
	set( RestRows, NewRestRows, [X, Y1], Value). 

% setInList( List, NewList, Index, Value) 

setInList( [_|RestList], [Value|RestList], 0, Value). 

setInList( [Element|RestList], [Element|NewRestList], Index, Value) :- 
	Index > 0, 
	Index1 is Index-1, 
	setInList( RestList, NewRestList, Index1, Value). 
 
