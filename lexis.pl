% lexis.pl


:-consult('support').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Question 1 (5%)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% read_move(-Move) ... [or read_move(-Move, +Input, ?Remainder) as a non-DCG rule]
%   Given an Input string of the form:
%     "xy"        - where x is a valid column and y is a valid row, e.g. "e6"
%   or
%     "xyd" - where x is a valid column
%                   y is a valid row
%                   d is either "v" or "h", e.g. "c6h"
%   Returns the corresponding internal representation of the move as Move

read_move(Move) --> letter(X), number(Y), {
  Move = (X,Y)
}.

read_move(Move) --> letter(X), number(Y), hfenceletter, {
  Move = (X,Y,h)
}.

read_move(Move) --> letter(X), number(Y), vfenceletter, {
  Move = (X,Y,v)
}.

number(X) --> [C], {
  "1"=<C,
  C=<"9",
  X is C - "0"
}.

letter(X) --> [C], {
  C>=97,
  C=<105,
  convert(C,Number),
  X is Number
}.

hfenceletter --> [104].
vfenceletter --> [118].

convert(97, 1).
convert(98, 2).
convert(99, 3).
convert(100, 4).
convert(101, 5).
convert(102, 6).
convert(103, 7).
convert(104, 8).
convert(105, 9).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Question 2 (50%)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% 2 (a) in_range(?N,+Min,+Max)

% Uncomment the following to skip this part.
/*
:- use_module(library(between)).

in_range(N, Min, Max):-
  between(Min, Max, N).
*/

in_range(N, Min, Max) :-
  Min =< Max,
  NewMin is Min + 1,
  in_range(N, NewMin, Max).

in_range(Min, Min, Max) :-
  Min =< Max.


% 2(b) fence_space(+Fs,?Space)
%   Given the list Fs of existing fences,
%   Space is a fence space, i.e. a new fence Space = (X,Y,Dir)
%   could be added, ignoring the paths open to either pawn.

% Uncomment the following incomplete program to skip this part.
/*
fence_space(_, (1,1,h)).
fence_space(_, (1,1,v)).
*/

% go through whole board and check for each 2x2 square whether fence can be laid
% - only if there is no fence in the middle of it - neither a horizontal or vertical
fence_space(Fs, Space) :-
  in_range(X, 1, 8),
  in_range(Y, 1, 8),
  compute_fence_space(Fs, X, Y, Space).

% lay vertical fence
compute_fence_space(Fs, X, Y, (X,Y,v)) :-
  NewY is Y + 1,
  \+ member((X,Y,h), Fs),
  \+ v_fence_at(X,Y,Fs), % succeeds if there is a vertical fence between (X,Y) and (X+1,Y)
  \+ v_fence_at(X,NewY,Fs).

% lay horizontal fence
compute_fence_space(Fs, X, Y, (X,Y,h)) :-
  NewX is X + 1,
  \+ member((X,Y,v), Fs),
  \+ h_fence_at(X,Y,Fs), % succeeds if there is a horizontal fence between (X,Y) and (X,Y+1)
  \+ h_fence_at(NewX,Y,Fs).


% 2(c) reachable(+From,+Fences,?To)
%   Succeeds iff To = (X1,Y1) is reachable by a pawn at From = (X,Y)
%   with the given list of Fences, ignoring the position of the other pawn.

% Uncomment the following incomplete program to skip this part.
/*
reachable(_, _, (1,1)).
reachable(_, _, (9,9)).
*/

% can reach all adjacent squares
% store current state
% check for adjacent nodes reachable from current state
% store already reached nodes so these are not revisited in recursive calls
reachable(From, Fences, To) :-
  compute_reachable([From], Fences, [], [], To).

compute_reachable([(X,Y)|Tail], Fences, NextToVisit, AlreadyVisited, To) :-
  findall((X2,Y2), (can_reach_in_one((X,Y), Fences, (X2,Y2)), \+ member((X2,Y2), AlreadyVisited), \+ member((X2,Y2), NextToVisit)), L),
  append(NextToVisit, L, NewNextToVisit),
  compute_reachable(Tail, Fences, NewNextToVisit, [(X,Y)|AlreadyVisited], To).

compute_reachable([], Fences, NextToVisit, AlreadyVisited, To) :-
  length(NextToVisit, N),
  N > 0,
  compute_reachable(NextToVisit, Fences, [], AlreadyVisited, To).

compute_reachable([Curr|_], _, _, _, Curr).

comp(From, Fences, To, Size) :-
  findall(To, reachable(From, Fences, To), L),
  length(L, Size).


% can go up
can_reach_in_one((X,Y), Fences, (X,NewY)) :-
  Y > 1,
  NewY is Y - 1,
  in_range(X, 1, 9),
  in_range(NewY, 1, 9),
  \+ h_fence_at(X, NewY, Fences).

% can go right
can_reach_in_one((X,Y), Fences, (NewX,Y)) :-
  X < 9,
  NewX is X + 1,
  in_range(NewX, 1, 9),
  in_range(Y, 1, 9),
  \+ v_fence_at(X, Y, Fences).

% can go down
can_reach_in_one((X,Y), Fences, (X,NewY)) :-
  Y < 9,
  NewY is Y + 1,
  in_range(X, 1, 9),
  in_range(NewY, 1, 9),
  \+ h_fence_at(X, Y, Fences).

% can go left
can_reach_in_one((X,Y), Fences, (NewX,Y)) :-
  X > 1,
  NewX is X - 1,
  in_range(NewX, 1, 9),
  in_range(Y, 1, 9),
  \+ v_fence_at(NewX, Y, Fences).



% 2(d) fence_move(NumFs, Fences, (OppX,OppY), OppTarget, NewF)
%   Given NumFs, the number of fences the player has left to play,
%   the list Fs = [(X,Y,Dir),...] of fences on the board,
%   (OppX,OppY) the position of the opposing pawn, and
%   OppTarget, the row the opponent is trying to reach,
%   returns a fence = (X,Y,Dir) that can be added according to the rules.


% check for each fence in fence space if it is not covering the opponent pawn
% check also if it is still making it possible for opponent to reach destination row

fence_move(NumFs, Fences, (OppX,OppY), OppTarget, NewF) :-
  NumFs > 0,
  setof(NewF, get_fence(Fences, (OppX,OppY), OppTarget, NewF), FenceList),
  member(NewF, FenceList).

get_fence(Fences, (OppX,OppY), OppTarget, NewF) :-
  fence_space(Fences,NewF),
  reachable((OppX,OppY),[NewF|Fences],(_,OppTarget)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Question 3 (20%)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% pawn_move(+OpSq,+Fs,+From,?To).
%   Given pairs OpSq = (OppX,OppY) and From = (X,Y) defining the current
%   locations of the two pawns, and a list Fs = [(F1X,F1Y,F1Dir),...],
%   returns a valid square To = (X1,Y1) that the pawn at (X,Y) can move to
%   in a single turn.

% Uncomment the following to skip this Question.
/*
pawn_move(_, _, _, (5,2)).
pawn_move(_, _, _, (6,1)).
pawn_move(_, _, _, (4,1)).
*/

% if opp pawn is above we can jump to square above opp pawn if not blocked by fence
% if this blocked by fence then

% L-shaped jump up to the left --------------------------------------------------
pawn_move((OppX,OppY), Fs, (X,Y), (NewX,NewY)) :-
  X > 1,
  OppX == X,
  NewY is Y-1,
  NewX is X-1,
  OppY == NewY,
  NewOppY is OppY - 1,
  h_fence_at(OppX,NewOppY,Fs),
  \+ v_fence_at(NewX,OppY,Fs).

% L-shaped jump up to the right
pawn_move((OppX,OppY), Fs, (X,Y), (NewX,NewY)) :-
  X < 9,
  NewY is Y-1,
  OppX == X,
  OppY == NewY,
  NewOppY is OppY - 1,
  h_fence_at(OppX,NewOppY,Fs),
  NewX is X+1,
  \+ v_fence_at(X,OppY,Fs).


% L-shaped jump right and up
pawn_move((OppX,OppY), Fs, (X,Y), (NewX,NewY)) :-
  Y > 1,
  NewX is X+1,
  OppX == NewX,
  OppY == Y,
  v_fence_at(OppX,OppY,Fs),
  NewY is Y-1,
  \+ h_fence_at(NewX,NewY,Fs).

% L-shaped right and down
pawn_move((OppX,OppY), Fs, (X,Y), (NewX,NewY)) :-
  Y < 9,
  NewX is X + 1,
  OppX == NewX,
  OppY == Y,
  v_fence_at(OppX,OppY,Fs),
  NewY is Y + 1,
  \+ h_fence_at(NewX,Y,Fs).

% L-shaped down to the right
pawn_move((OppX,OppY), Fs, (X,Y), (NewX,NewY)) :-
  Y < 9,
  NewY is Y+1,
  OppX == X,
  OppY == NewY,
  h_fence_at(OppX,OppY,Fs),
  NewX is X+1,
  \+ v_fence_at(X,OppY,Fs).


% L-shaped down to the left
pawn_move((OppX,OppY), Fs, (X,Y), (NewX, NewY)) :-
  Y > 1,
  NewX is X-1,
  OppX == NewX,
  OppY == Y,
  h_fence_at(OppX,OppY,Fs),
  NewY is Y+1,
  \+ v_fence_at(NewX,OppY,Fs).


% L-shaped jump left and up
pawn_move((OppX,OppY), Fs, (X,Y), (NewX,NewY)) :-
  Y > 1,
  NewX is X - 1,
  OppX == NewX,
  OppY == Y,
  NewOppX is OppX - 1,
  v_fence_at(NewOppX,OppY,Fs),
  NewY is Y - 1,
  \+ h_fence_at(NewX,NewY,Fs).

% L-shaped jump left and down
pawn_move((OppX,OppY), Fs, (X,Y), (NewX,NewY)) :-
  Y < 9,
  NewX is X-1,
  OppX == NewX,
  OppY == Y,
  NewOppX is OppX - 1,
  v_fence_at(NewOppX,OppY,Fs),
  NewY is Y+1,
  \+ h_fence_at(NewX,Y,Fs).

% jump up
pawn_move((OppX,OppY), Fs, (X,Y), (X,NewY)) :-
  Y > 2,
  Y2 is Y - 1,
  OppX == X,
  OppY == Y2,
  NewOppY is OppY - 1,
  \+ h_fence_at(OppX,NewOppY,Fs),
  NewY is Y - 2.

% jump right
pawn_move((OppX,OppY), Fs, (X,Y), (NewX,Y)) :-
  X < 8,
  X2 is X + 1,
  OppX == X2,
  OppY == Y,
  \+ v_fence_at(OppX,OppY,Fs),
  NewX is X + 2.

% jump down
pawn_move((OppX,OppY), Fs, (X,Y), (X,NewY)) :-
  Y < 8,
  Y2 is Y + 1,
  OppX == X,
  OppY == Y2,
  \+ h_fence_at(OppX,OppY,Fs),
  NewY is Y + 2.

% jump left
pawn_move((OppX,OppY), Fs, (X,Y), (NewX,Y)) :-
  X > 2,
  X2 is X - 1,
  OppX == X2,
  OppY == Y,
  NewOppX is OppX - 1,
  \+ v_fence_at(NewOppX,OppY,Fs),
  NewX is X - 2.

% move up
pawn_move((OppX,OppY), Fs, (X,Y), (X,NewY)) :-
  Y > 1,
  NewY is Y - 1,
  OppY \== NewY,
  \+ h_fence_at(X,NewY,Fs).

% move right
pawn_move((OppX,OppY), Fs, (X,Y), (NewX,Y)) :-
  X < 9,
  NewX is X + 1,
  OppX \== NewX,
  \+ v_fence_at(X,Y,Fs).

% move down
pawn_move((OppX,OppY), Fs, (X,Y), (X,NewY)) :-
  Y < 9,
  NewY is Y + 1,
  OppY \== NewY,
  \+ h_fence_at(X,Y,Fs).

% move left
pawn_move((OppX,OppY), Fs, (X,Y), (NewX,Y)) :-
  X > 1,
  NewX is X - 1,
  OppX \== NewX,
  \+ v_fence_at(NewX,Y,Fs).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Question 4 (15%)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% 4 (a) game_over(+State)
%   Given a board state State, succeeds if one of the players has won the game.


% board state is list [P1State, P2State, Fences]
% P1State is p1(X1,Y1,Fences1)
game_over([p1(_,9,_), P2State, Fences]).
game_over([P1State, p2(_,1,_), Fences]).



% 4 (b) next_state(+S0,+Player,+Move,?S1).
%   Returns S1, given S0, Player and Move.
%   S0 and S1 are states.
%   Move is either (X,Y) or (X,Y,Dir)
%   Player is either p1 or p2.
next_state([p1(X,Y,Z), P2State, Fences], p1, (A,B,C), S1) :- !,
  NewZ is Z - 1,
  S1 = [p1(X,Y,NewZ), P2State, [(A,B,C)|Fences]].

next_state([P1State, p2(X,Y,Z), Fences], p2, (A,B,C), S1) :- !,
  NewZ is Z - 1,
  S1 = [P1State, p2(X,Y,NewZ), [(A,B,C)|Fences]].

next_state([p1(_,_,Z), P2State, Fences], p1, (X,Y), [p1(X,Y,Z), P2State, Fences]).
next_state([P1State, p2(_,_,Z), Fences], p2, (X,Y), [P1State, p2(X,Y,Z), Fences]).



% 4 (c) play(+Player,+Name,+OppName,+State,-Winner)
%  Given Player (either p1 or p2) identifying the next player to take a turn,
%  their name Name, their opponent's name OppName, and the current board State,
%  returns the winner's name as Winner.

play(Player, Name, OppName, State, Winner) :-
  game_over(State),!,
  show_board(State),
  determine_winner(State, Winner).

play(Player, Name, OppName, State, Winner) :-
  show_board(State),
  select_move(Player, Name, State, Move),
  next_state(State, Player, Move, S1),
  (Player == p1 -> play(p2, OppName, Name, S1, Winner);
  play(p1,OppName, Name, S1, Winner)).

determine_winner([p1(_,9,_), _, _], p1).
determine_winner([_, p2(_,1,_), _], p2).
