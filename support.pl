% support.pl
% Author:   Tim Kimber

% This file provides example states for testing
% and definitions for the provided parts of the Quoridor program.
% Your file lexis.pl should consult this file.
% You may add more examples to this file, or modify the existing examples 
% if you wish but you should not make any other changes.
% This file does NOT form part of your submission and will not be marked.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% EXAMPLE STATES FOR TESTING
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% prints all examples
print_examples :-
  test_state(_, _S),
  show_board(_S),
  fail.
  
% initial state
test_state(s1,
  [
    p1(5,1,10), 
    p2(5,9,10), 
    []
  ]).

% the state in Fig 2.
test_state(s2, 
  [
    p1(6,5,7), 
    p2(6,6,5), 
    [(2,3,v), (2,5,v), (3,2,h), (4,3,v), (5,4,h), (6,4,v), (4,7,h), (6,7,h)]
  ]).
  
% the state in Fig. 3
test_state(s3, 
  [
    p1(5,1,8), 
    p2(3,7,9), 
    [(4,3,v), (4,7,h), (6,7,h)]
  ]).
  
% the state in Fig. 4
test_state(s4,
  [
    p1(5,1,8),
    p2(2,2,9),
    [(2,1,v), (1,2,h)]
  ]).
  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% GAME ENGINE
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% If play/5 and legal_move/3 have been implemented,
% calling this predicate from the Prolog prompt will start the game.
% Name1, Name2 are the names the players.

quoridor(Name1,Name2):-
    initial_state(S0),
    show_board(S0),
    play(p1,Name1,Name2,S0,Winner),
    !,
    print_result(Winner).

initial_state([p1(5,1,10),p2(5,9,10),[]]).

print_result(Winner):-
    format('Game over. ~a is the winner.~n',[Winner]).


% select_move(Player,Name,State,Move)
%    Prompts Name for the next move for Player (p1 or p2)
%    and translates the move from human readable syntax to its 
%    Prolog representation.
%    If the move is legal it is returned in its Prolog form as Move,
%    otherwise the player is prompted to try again.
%    Moves are entered using the Quoridor notation:
%        e6.  means "move pawn to square e6"
%        b4v. means "add a fence dividing squares b4,b5,c4 and c5 vertically".
%    An entered move must finish with a '.', or Prolog will just wait for
%    one to be entered.
%    select_move depends on the answers to Q1-Q3.
select_move(Player,Name,S0,Move):-
    write(Name),
    write(', please enter your next move'),nl,
    flush_output(user),
    catch(read(Term), _, Term = _),
    (
      atom(Term),
      atom_codes(Term, String),
      phrase(read_move(PossMove), String)
    ->
        (legal_move(Player,S0,PossMove) ->
            Move=PossMove
         ;
            write('That move is not allowed.'),nl,
            select_move(Player,Name,S0,Move)
        )
    ; 
        Term == end_of_file
    ->
        false
    ;
        write('Move not recognised.'),nl,
        select_move(Player,Name,S0,Move)
    ).


% legal_move(+Player,+S,?Move)
%   Player is either p1 or p2
%   S is the current state
%   Move is some legal move for Player in S

legal_move(p1,S0,(X,Y)):-
    S0 = [p1(X0,Y0,_),p2(OppX,OppY,_),Fs],
    pawn_move((OppX,OppY),Fs,(X0,Y0),(X,Y)).

legal_move(p2,S0,(X,Y)):-
    S0 = [p1(OppX,OppY,_),p2(X0,Y0,_),Fs],
    pawn_move((OppX,OppY),Fs,(X0,Y0),(X,Y)).

legal_move(p1,S0,NewF):-
    S0 = [p1(_,_,F1),p2(X2,Y2,_),Fs],
    fence_move(F1, Fs, (X2,Y2), 1, NewF).
      
legal_move(p2,S0,NewF):-
    S0 = [p1(X1,Y1,_),p2(_,_,F2),Fs],
    fence_move(F2, Fs, (X1,Y1), 9, NewF).
       

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% GAME OUTPUT SECTION
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% For displaying the current state of the game
    
show_board(S):-
    nl,
    format('              a   b   c   d   e   f   g   h   i~n',[]),
    show_border(S, 0.5),
    show_rows(S, 1),
    nl.
    
show_rows(_, 10):-!.
show_rows(S, Y):-
    show_row(S, Y),
    show_border(S, Y),
    Next is Y + 1,
    show_rows(S, Next).
    
show_row([p1(X1,Y1,F1), p2(X2,Y2,F2), Fs], Y):-
    Line is Y * 2,
    show_fence(p1, F1, Line),
    show_row_label(Y),
    show_squares([p1(X1,Y1,F1), p2(X2,Y2,F2), Fs], 1, Y),
    show_fence(p2, F2, Line),
    nl.
    
show_squares(_,10,_):-!.
show_squares([p1(X1,Y1,F1),p2(X2,Y2,F2),Fs],X,Y):-
    ((X,Y) == (X1,Y1) -> 
        Pawn = 'O'
     ;
        ((X,Y) == (X2,Y2) ->
            Pawn = 'X'
         ;
            Pawn = ' '
        )
    ),
    ((X == 9 ; v_fence_at(X,Y,Fs)) ->
        Border = '|'
     ;
        Border = ' '
    ),
    format(' ~a ~a',[Pawn,Border]),
    Next is X + 1,
    show_squares([p1(X1,Y1,F1),p2(X2,Y2,F2),Fs],Next,Y).
    
show_border([p1(X1,Y1,F1),p2(X2,Y2,F2),Fs],Y):-
    Line is Y * 2 + 1,
    show_fence(p1,F1,Line),
    write('     +'),
    show_ends([p1(X1,Y1,F1),p2(X2,Y2,F2),Fs],1,Y),
    show_fence(p2,F2,Line),
    nl.

show_ends(_,10,_):-!.
show_ends([P1,P2,Fs],X,Y):-
    (h_fence_at(X,Y,Fs) ->
        atom_codes('-',[Fill])
     ;
        ((Y == 0.5;Y == 9) ->
            atom_codes('-',[Fill])
         ;
            atom_codes(' ',[Fill])
        )
    ),
    format('~|~*t~3++',[Fill]),
    Next is X + 1,
    show_ends([P1,P2,Fs],Next,Y).


show_fence(p1,NF,Line):-
    (NF >= Line ->
        write('    ---')
     ;
        write('       ')
    ).

show_fence(p2,NF,Line):-
    (NF >= (20 - Line) ->
        write(' ---')
    ;
        true
    ).
    
show_row_label(Y):-
    format('  ~d  |',[Y]).    


% v_fence_at(X,Y,Fs)     
%   given integers X and Y and the list of fences Fs,
%   succeeds if there is a vertical fence between (X,Y) and (X+1,Y)
v_fence_at(X,Y,Fs):-
    member((X,Y,v),Fs), !.
v_fence_at(X,Y,Fs):-
    YUp is Y - 1,
    member((X,YUp,v),Fs).

% h_fence_at(X,Y,Fs)     
%   given integers X and Y and the list of fences Fs,
%   succeeds if there is a horizontal fence between (X,Y) and (X,Y+1)    
h_fence_at(X,Y,Fs):-
    member((X,Y,h),Fs), !.    
h_fence_at(X,Y,Fs):-
    XLeft is X - 1,
    member((XLeft,Y,h),Fs).

