%:- use_module(library(tabling)).
%:- table move/3.
%:- table get_best_move1/3.
%solve([1,2,3,0,5,6,4,7,8],[],[1,2,3,4,5,6,7,8,0]).
%solution([1,2,3,7,4,6,0,5,8],X).
%trace,Y=[1,2,3,0,5,6,4,7,8],X=[1,2,3,4,5,6,7,8,0],findall((Move,XL),move(Y,Move,XL),List),get_all_distances(List,X,Dist),Dist=[(_,_,H)|_],get_best_move(Dist,H,Q),get_best_move1(Q,Dist,(Goal1,Move1)).
%solve([1,2,3,7,4,6,0,5,8],[],[1,2,3,4,5,6,7,8,0]).
%solve(1/2/3/7/4/6/0/5/8,X).
%p_fcn([A,B,C,D,E,F,G,H,I], P) :-
 %    a(A,Pa), b(B,Pb), c(C,Pc),
  %   d(D,Pd), e(E,Pe), f(F,Pf),
   %  g(G,Pg), h(H,Ph), i(I,Pi),
    % P is Pa+Pb+Pc+Pd+Pe+Pf+Pg+Ph+Pg+Pi.


goal([1,2,3,4,5,6,7,8,0]).
%goal([1,2,3,8,0,4,7,6,5]).

%solve([0,1,3,4,2,5,7,8,6],[],[1,2,3,4,5,6,7,8,0]).
%solve([4,1,3,0,2,5,7,8,6],[],[1,2,3,4,5,6,7,8,0]).
no_of_elements_in_og_position([A,B,C,D,E,F,G,H,I],Number):-
    position(X,A),position(X1,B),position(X2,C),
    position(X3,D),position(X4,E),position(X5,F),
    position(X6,G),position(X7,H),position(X8,I),
    basics:ith(Index,[A,B,C,D,E,F,G,H,I],1),
    basics:ith(Index1,[A,B,C,D,E,F,G,H,I],2),
    basics:ith(Index2,[A,B,C,D,E,F,G,H,I],3),
    basics:ith(Index3,[A,B,C,D,E,F,G,H,I],4),
    basics:ith(Index4,[A,B,C,D,E,F,G,H,I],5),
    basics:ith(Index5,[A,B,C,D,E,F,G,H,I],6),
    basics:ith(Index6,[A,B,C,D,E,F,G,H,I],7),
    basics:ith(Index7,[A,B,C,D,E,F,G,H,I],8),
    basics:ith(Index8,[A,B,C,D,E,F,G,H,I],0),
    (
        X=:=Index->
            Count = 0
            ;
            Count = 1

    )
    ,
    (
        X1=:=Index1->
            Count1 = 0
            ;
            Count1 = 1

    )
    ,
    (
        X2=:=Index2->
            Count2 = 0
            ;
            Count2 = 1

    )
    ,
    (
        X3=:=Index3->
            Count3 = 0
            ;
            Count3 = 1

    )
    ,
    (
        X4=:=Index4->
            Count4 = 0
            ;
            Count4 = 1

    )
    ,
    (
        X5=:=Index5->
            Count5 = 0
            ;
            Count5 = 1

    )
    ,
    (
        X6=:=Index6->
            Count6 = 0
            ;
            Count6 = 1

    )
    ,
    (
        X7=:=Index7->
            Count7 = 0
            ;
            Count7 = 1

    )
    ,
    (
        X8=:=Index8->
            Count8 = 0
            ;
            Count8 = 1

    )
    ,
    Number is  Count + Count1+Count2 + Count3+Count4 + Count5+Count6 + Count7+Count8.  

position(1,1).
position(2,2).
position(3,3).
position(4,4).
position(5,5).
position(6,6).
position(7,7).
position(8,8).
position(9,0).

grid(1,1,1).
grid(1,2,2).
grid(1,3,3).
grid(2,1,4).
grid(2,2,5).
grid(2,3,6).
grid(3,1,7).
grid(3,2,8).
grid(3,3,0).

get_grid_index(Val_from_ith,A,B):-
    Val_from_ith < 4,
    A is 1,
    B is Val_from_ith.


get_grid_index(Val_from_ith,A,B):-
    Val_from_ith <7,
    Val_from_ith > 3,
    A is 2,
    B is Val_from_ith - A - 1.


get_grid_index(Val_from_ith,A,B):-
    Val_from_ith >6,
    A is 3,
    B is Val_from_ith - 2*A.

dist1([A,B,C,D,E,F,G,H,I],S):-
    position(X,A),position(X1,B),position(X2,C),
    position(X3,D),position(X4,E),position(X5,F),
    position(X6,G),position(X7,H),position(X8,I),
    basics:ith(Index,[A,B,C,D,E,F,G,H,I],1),
    basics:ith(Index1,[A,B,C,D,E,F,G,H,I],2),
    basics:ith(Index2,[A,B,C,D,E,F,G,H,I],3),
    basics:ith(Index3,[A,B,C,D,E,F,G,H,I],4),
    basics:ith(Index4,[A,B,C,D,E,F,G,H,I],5),
    basics:ith(Index5,[A,B,C,D,E,F,G,H,I],6),
    basics:ith(Index6,[A,B,C,D,E,F,G,H,I],7),
    basics:ith(Index7,[A,B,C,D,E,F,G,H,I],8),
    basics:ith(Index8,[A,B,C,D,E,F,G,H,I],0),
    S is abs(X-Index)+abs(X1-Index1)+abs(X2-Index2)+abs(X3-Index3)+abs(X4-Index4)+abs(X5-Index5)+abs(X6-Index6)+abs(X7-Index7).
    

dist2([A,B,C,D,E,F,G,H,I],Manhattan):-
    grid(X1,Y1,A),grid(X2,Y2,B),grid(X3,Y3,C),grid(X4,Y4,D),grid(X5,Y5,E),grid(X6,Y6,F),grid(X7,Y7,G),grid(X8,Y8,H),grid(X9,Y9,I),
    basics:ith(Index,[A,B,C,D,E,F,G,H,I],1),
    basics:ith(Index1,[A,B,C,D,E,F,G,H,I],2),
    basics:ith(Index2,[A,B,C,D,E,F,G,H,I],3),
    basics:ith(Index3,[A,B,C,D,E,F,G,H,I],4),
    basics:ith(Index4,[A,B,C,D,E,F,G,H,I],5),
    basics:ith(Index5,[A,B,C,D,E,F,G,H,I],6),
    basics:ith(Index6,[A,B,C,D,E,F,G,H,I],7),
    basics:ith(Index7,[A,B,C,D,E,F,G,H,I],8),
    basics:ith(Index8,[A,B,C,D,E,F,G,H,I],0),
    get_grid_index(Index,A1,B1),get_grid_index(Index1,A2,B2),get_grid_index(Index2,A3,B3),get_grid_index(Index3,A4,B4),get_grid_index(Index4,A5,B5),get_grid_index(Index5,A6,B6),get_grid_index(Index6,A7,B7), get_grid_index(Index7,A8,B8),get_grid_index(Index8,A9,B9),
    Manhattan is abs(X1-A1) + abs(Y1-B1) + abs(X2-A2) + abs(Y2-B2) + abs(X3-A3) + abs(Y3-B3) + abs(X4-A4) + abs(Y4-B4) + abs(X5-A5) + abs(Y5-B5) + abs(X6-A6) + abs(Y6-B6) + abs(X7-A7) + abs(Y7-B7)+abs(X8-A8) + abs(Y8-B8).
    

   %%% the out-of-cycle function
s_fcn([A,B,C,D,E,F,G,H,I], S) :-
     s_aux(A,B,S1), s_aux(B,C,S2), s_aux(C,F,S3),
     s_aux(F,I,S4), s_aux(I,H,S5), s_aux(H,G,S6),
     s_aux(G,D,S7), s_aux(D,A,S8), s_aux(E,S9),
     S is S1+S2+S3+S4+S5+S6+S7+S8+S9.

s_aux(0,0) :- !.
s_aux(_,1).

s_aux(X,Y,0) :- Y is X+1, !.
s_aux(8,1,0) :- !.
s_aux(_,_,2).

distance(L,X,P):-
    dist2(L,P1),
    s_fcn(L,S1),
    no_of_elements_in_og_position(L,O1),
    P is P1+3*O1.

:- dynamic(seen/1).
solver(StartState):-
    goal(GoalState),
    solve(StartState,[],GoalState).
solve(X,_,X):-!.
solve(Y,L,X):-
    basics:append(L,[(Y,_)],L1),
    assert(seen(Y)),
    distance(Y,X,Previous),
    !,
    findall((Move,XL),(move(Y,Move,XL),\+seen(XL)),List),
    get_all_distances(List,X,Dist),
    Dist=[(_,_,H)|_],
    get_best_move(Dist,H,Q),
    get_best_move1(Q,Dist,(Best,Move1)),
    \+best_in_L(Best,L1),
    basics:append(L1,[(Best,Move1)],L2),
    %writeln(L1),
    Best = [Cell1,Cell2,Cell3,Cell4,Cell5,Cell6,Cell7,Cell8,Cell9],
    writeln([Cell1,Cell2,Cell3]),
    writeln([Cell4,Cell5,Cell6]),
    writeln([Cell7,Cell8,Cell9]),
    writeln('---------------- '),
    writeln('----------------'),
    solve(Best,L2,X).

best_in_L(_,[]):-fail.
best_in_L(Config,[(Config,_)|_]).
best_in_L(Config,[(_,_)|T]):-
    best_in_L(Config,T).

get_all_distances([],_,[]).
get_all_distances([(Move,Goal)|T],X,[(Goal,Move,N1)|Dist]):-
    distance(Goal,X,N1),
    get_all_distances(T,X,Dist).

get_best_move([],N,N).
get_best_move([(_,_,Min)|T],N,N2):-
    (   N>Min->
        N1=Min,
        get_best_move(T,N1,N2)
    ;   
    get_best_move(T,N,N2)
    ).

get_best_move1(N,[(Goal,Move,N)|_],(Goal,Move)).
get_best_move1(N,[_|T],(Goal,Move)):-
    get_best_move1(N,T,(Goal,Move)).

    

member(H,[H|_]).
member(H,[_|T]):-
    member(H,T).

    
    
            
get_valid_move(0,right).
get_valid_move(0,left).
get_valid_move(0,up).
get_valid_move(0,down).
get_valid_move(right,right).
get_valid_move(right,up).
get_valid_move(right,down).
get_valid_move(left,left).
get_valid_move(left,up).
get_valid_move(left,down).
get_valid_move(up,up).
get_valid_move(up,left).
get_valid_move(up,right).
get_valid_move(down,down).
get_valid_move(down,left).
get_valid_move(down,right).

previous([],0).
previous(L,H1):-
    reverse(L,L1),
    L1=[H1|_].




move([A,B,C,D,E,F,G,H,0],left,[A,B,C,D,E,F,G,0,H]).
move([A,B,C,D,E,F,G,0,H],left,[A,B,C,D,E,F,0,G,H]).
move([A,B,C,D,E,0,G,H,F],left,[A,B,C,D,0,E,G,H,F]).
move([A,B,C,D,0,E,G,H,F],left,[A,B,C,0,D,E,G,H,F]).
move([A,B,0,D,E,F,G,H,C],left,[A,0,B,D,E,F,G,H,C]).
move([A,0,B,D,E,F,G,H,C],left,[0,A,B,D,E,F,G,H,C]).

move([A,B,C,D,E,F,0,G,H],right,[A,B,C,D,E,F,G,0,H]).
move([A,B,C,D,E,F,G,0,H],right,[A,B,C,D,E,F,G,H,0]).
move([A,B,C,0,E,F,G,H,D],right,[A,B,C,E,0,F,G,H,D]).
move([A,B,C,E,0,F,G,H,D],right,[A,B,C,E,F,0,G,H,D]).
move([0,B,C,D,E,F,G,H,A],right,[B,0,C,D,E,F,G,H,A]).
move([B,0,C,D,E,F,G,H,A],right,[B,C,0,D,E,F,G,H,A]).

move([A,B,C,D,E,F,G,H,0],up,[A,B,C,D,E,0,G,H,F]).
move([A,B,C,D,E,0,G,H,F],up,[A,B,0,D,E,C,G,H,F]).
move([A,B,C,D,E,F,G,0,H],up,[A,B,C,D,0,F,G,E,H]).
move([A,B,C,D,0,F,G,E,H],up,[A,0,C,D,B,F,G,E,H]).
move([A,B,C,D,E,F,0,G,H],up,[A,B,C,0,E,F,D,G,H]).
move([A,B,C,0,E,F,D,G,H],up,[0,B,C,A,E,F,D,G,H]).

move([A,B,0,D,E,F,G,H,C],down,[A,B,F,D,E,0,G,H,C]).
move([A,B,F,D,E,0,G,H,C],down,[A,B,F,D,E,C,G,H,0]).
move([A,0,C,D,E,F,G,H,B],down,[A,E,C,D,0,F,G,H,B]).
move([A,E,C,D,0,F,G,H,B],down,[A,E,C,D,H,F,G,0,B]).
move([0,B,C,D,E,F,G,H,A],down,[D,B,C,0,E,F,G,H,A]).
move([D,B,C,0,E,F,G,H,A],down,[D,B,C,G,E,F,0,H,A]).