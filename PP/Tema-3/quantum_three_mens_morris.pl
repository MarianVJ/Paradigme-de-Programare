% Quantum Three Men's Morris

% Se propune o variantă cuantică a jocului "Three Men's Morris", joc
% în care doi adversari plasează și apoi mută câte trei piese pe o
% tablă cu 3x3 celule. Un jucător va avea trei piese albe, iar cel
% de-al doilea trei piese negre. Scopul fiecăruia este de a-și aranja
% piesele pe aceeași linie, pe aceeași coloană sau pe aceeași
% diagonală.
%
%  (1,1) -- (1,2) -- (1,3)
%    |    \   |    /   |
%    |     \  |   /    |
%  (2,1) -- (2,2) -- (2,3)
%    |     /  |   \    |
%    |    /   |    \   |
%  (3,1) -- (3,2) -- (3,3)
%
% Pe tablă sunt 9 celule.
%
% Jocul are două etape:
%
%  i. Plasarea pieselor
%
%     Alternativ, fiecare jucător va plasa câte o piesă în stare
%     cuantică pe tablă. Asta presupune alegerea a două celule în care
%     NU se află o piesă în stare clasică. Cele două celule vor deveni
%     legate la nivel cuantic (eng. entangled).
%
%     Atunci când se construiește un ciclu de celule legate cuantic
%     (entangled) jucătorul următor (nu cel care a creat ciclul) va
%     "măsura" ("observa") poziția ultimei piese plasate pe tablă (cea
%     care închis ciclul) și va alege în care dintre cele două celule
%     va rămâne aceasta. Observarea unei poziții duce la colapsarea
%     întregii componente a grafului din care face parte ciclul.
%
%     Etapa de plasare a pieselor se va termina atunci când fiecare
%     dintre cei doi jucători are câte trei piese indiferent în ce
%     stare.  (Se poate produce un ciclu în această etapă sau nu.)
%
% ii. Mutarea pieselor
%
%     Alternativ, fiecare jucător alege o piesă pe care să o mute
%     într-o celulă liberă (în care nu se află o piesă în stare
%     clasică). Dacă piesa se află în stare cuantică, atunci ambele
%     celule posibile se vor schimba. Dacă piesa se alfă în stare
%     clasică, atunci se va indica o pereche de celule vecine, iar
%     piesa va ajunge într-o stare cuantică. Efectul unei mutări lasă
%     piesa mutată în stare cuantică, iar cele două celule posibile
%     sunt, desigur, legate la nivel cuantic.
%
%     Atunci când se construiește un ciclu de celule legate cuantic
%     jucătorul următor (nu cel care a creat ciclul) va "măsura"
%     poziția ultimei piese mutate (cea care a închis ciclul) și va
%     alege în care dintre cele două celule posibile va rămâne
%     aceasta. Observarea unei poziții poate duce la observarea
%     pozițiilor mai multor piese.
%
%     Jocul se încheie atunci când cel puțin unul dintre jucători are
%     trei piese în stare clasică pe aceeași linie, coloană sau
%     diagonală.
%
% Reprezentări folosite:
%
%  - O celulă va fi reprezentată printr-un tuplu pos(X,Y) unde X,Y
%    sunt 1, 2 sau 3.
%
%  - O piesă va fi reprezentată diferit în funcție de starea ei:
%     classic/2   e.g.  classic(pos(2,2), white)
%     quantum/3   e.g.  quantum(pos(1,2), pos(3,1), black)
%
%  - O stare va fi o listă de piese (maximum șase)
%     e.g.: [classic(pos(2,2), white), classic(pos(1,5), black),
%            quantum(pos(1,3), pos(2,3), white)]

% ----------------------------------------------------------------------

% Rezolvați pe rând cerințele de mai jos!

% [Cerința 1] Să se scrie predicatul next_player/2 care descrie
% alternanța culorilor jucătorilor. black îi urmează lui white, iar
% white îi urmează lui black.

%% next_player/2
next_player(black, white).
next_player(white, black).


% ----------------------------------------------------------------------

% [Cerința 2] Scrieți un predicat cell(?Cell) care să fie adevărat
% pentru atunci când Cell este o structură pos(X,Y) reprezentând o
% celulă de pe tablă.

% cell/1
 cell(pos(X,Y)):-  member(X,[1,2,3]), member(Y,[1,2,3]).


% ----------------------------------------------------------------------

% [Cerința 3] Scrieți un predicat valid_pairs(+State, +Color, -Pairs)
% care să descrie legătura dintre starea State și toate perechile de
% celule în care jucătorul Color ar putea așeza o piesă în stare
% cuantică. Celulele ocupate de piese în stare clasică nu reprezintă
% soluții valide. De asemenea, nici perechile de celule deja legate
% cuantic de o piesă a aceluiași jucător nu reprezintă perchi valide.
% Lista Pairs trebuie să NU conțină și o pereche și inversa ei.


%valid_pairs/3

valid_pairs(State, Color, Pairs):-
    findall((X,Y),(cell(X), cell(Y) , X @< Y ,
             \+ (member(quantum(X,Y,Color),State)),
             \+ (member(classic(X,_),State)), \+(member(classic(Y,_),State))),
            Pairs).



% ----------------------------------------------------------------------

% Cerința 4. Scrieți un predicat valid_moves(+State, +Color, -Moves)
% care leagă variabila Moves la lista tuturor mutărilor pe care le
% poate face jucătorul Color. O mutare poate fi de două feluri:
%
%  move(classic(pos(1,2), white), quantum(pos(1,3), pos(2,1), white))
%     sau
%  move(quantum(pos(3,3), pos(1,1), white),
%       quantum(pos(1,3), pos(2,1), white))


% valid_moves/3
%
%Poate doar daca sunt in sare clasica ,sa nu coincida cu poz initiala)

findMove(State,Color,classic(P,Color),Rez):-
    findall(quantum(X,Y,Color),(cell(X),cell(Y), \+ (member(quantum(X,Y,Color),State)),
                 \+ X == P,  \+ Y == P, X @< Y,
                \+ (member(classic(X,_),State)), \+(member(classic(Y,_),State)))
           , Rez).

findMove(State,Color,quantum(P1,P2,Color),Rez):-
    findall(quantum(X,Y,Color),(cell(X),cell(Y), \+ (member(quantum(X,Y,Color),State)), X @< Y,
                \+ X == P1, \+ Y == P1,\+ X == P2,\+ Y == P2,

                \+ (member(classic(X,_),State)), \+(member(classic(Y,_),State)))
           , Rez).

valid_moves(State, Color, Moves):-
    findall(move(Piece,NewPiece),
            (member(Piece,State), findMove(State,Color,Piece,Rez),
             member(NewPiece,Rez)
            ),Moves).


% ----------------------------------------------------------------------

% Cerința 5. Scrieți un predicat winner(+State, -Winner) care produce
% legarea variabilei Winner la lista jucătorilor care au cele trei
% piese în stare clasică aliniate. Dacă nimeni nu a câștigat, winner
% eșuează (nu leagă Winner la lista vidă).


% winner/2
%

linie([pos(A1,B1),pos(A2,B2),pos(A3,B3)]):- (A1 =:= A2, A2 =:= A3);
    (B1 =:= B2, B2 =:= B3);(A1 =1, B1 = 1,A2 = 2,B2 = 2,A3 = 3,B3 = 3);
    (A1 =1, B1 = 3,A2 = 2,B2 = 2,A3 = 3,B3 = 1).
linie(_):-false.

cauta(State,Color):-findall(P,member(classic(P,Color),State),List),
                length(List,3),linie(List).


winner(State, [white,black]):- cauta(State,white),cauta(State,black),!.
winner(State, [white]):- cauta(State,white).
winner(State, [black]):- cauta(State,black),!.
winner(_,_):- fail.

winner2(State, [white,black]):- cauta(State,white),cauta(State,black),!.
winner2(State, [white]):- cauta(State,white).
winner2(State, [black]):- cauta(State,black),!.
winner2(_,[]).


% ----------------------------------------------------------------------

% Cerința 6. Se cere scrierea unui predicat has_cycle(+State) care să
% fie adevărat dacă starea repsectivă conține un ciclu de celule
% legate cuantic.
%
% has_cycle([quantum(pos(1,1), pos(3,2), white),
%            quantum((2,1), (3,2), black),
%            quantum(pos(1,1), pos(2,1), white)])
%   => True.
%
% has_cycle([quantum(pos(1,1), pos(3,2), black),
%            quantum(pos(3,1), pos(3,2), white)])
%   => false.

% has_cycle/1
%
cycle_from(Y,State):-B = quantum(Y,Z,C),
              member(quantum(Y,Z,C),State),delete(State,B,State2),
              B2 = quantum(T,Z,C2),
              member(quantum(T,Z,C2),State2),
              delete(State2,B2,State3)
              ,cycle_from(Y,B2,State3).

cycle_from(Y,State):-B = quantum(Y,Z,C),
              member(quantum(Y,Z,C),State),delete(State,B,State2),
              B2 = quantum(Z,T,C2),
              member(quantum(Z,T,C2),State2),
              delete(State2,B2,State3)
              ,cycle_from(Y,B2,State3).

cycle_from(Start, quantum(_,Start,_),_):-!.
cycle_from(Start, quantum(Start,_,_),_):-!.

cycle_from(Start, quantum(_,Y,_),State):-(B = quantum(Y,Z,C),
              member(quantum(Y,Z,C),State),delete(State,B,State2)
,cycle_from(Start,B,State2);BB = quantum(Z,Y,CC),
              member(quantum(Z,Y,CC),State),delete(State,BB,State2)
,cycle_from(Start,BB,State2)   ).

cycle_from(Start, quantum(Y,_,_),State):-(B = quantum(Z,Y,C),
              member(quantum(Z,Y,C),State),delete(State,B,State2)
,cycle_from(Start,B,State2);B = quantum(Y,Z,C),
              member(quantum(Y,Z,C),State),delete(State,B,State2)
,cycle_from(Start,B,State2)).

has_cycle(State):-findall(Y,(cell(Y), cycle_from(Y,State)),L),
    length(L,Dim), Dim =\= 0.


% ----------------------------------------------------------------------

% Cerința 7. Se cere scrierea unui predicat collapse(+State, +Piece,
% +Cell, -NewState) care să producă starea obținută prin "măsurarea"
% piesei Piece în celula Cell. Starea NewState este rezulatul
% colapsării stării State. Piece este garantat un membru al lui State.

% collapse/4
%
%
%

getCol(quantum(_,_,C),C).


collapse_mod(State, A, Cell,Next):-getCol(A,C),
             delete(State,A,State2), State3 = [classic(Cell,C)|State2],
             findall((X,Aux),(member(quantum(Aux,Cell,B),State3), X = quantum(Aux,Cell,B)),Lista1),
             findall((X,Aux2),(member(quantum(Cell,Aux2,BB),State3), X = quantum(Cell,Aux2,BB)),Lista2),
             append(Lista1,Lista2,Lista),
             collapse_mod(State3,Lista,Next).

collapse_mod(State,[],State):-!.

collapse_mod(State,[(A,Cell)|Res],Next):- getCol(A,C),
    delete(State,A,State2), State3 = [classic(Cell,C)|State2],
    findall((X,Aux),(member(quantum(Aux,Cell,B),State3), X = quantum(Aux,Cell,B), \+member((X,_),Res)),Lista1),
    findall((X,Aux2),(member(quantum(Cell,Aux2,B2),State3), X = quantum(Cell,Aux2,B2), \+member((X,_),Res)),Lista2),
    append(Lista1,Lista2,Lista),
    append(Res,Lista,ResFinal),collapse_mod(State3,ResFinal,Next).





collapse(State, Piece, Cell, NewState):- collapse_mod(State,Piece,Cell,NewState).


% ----------------------------------------------------------------------
% ----------------------------------------------------------------------


% Un jucător trebuie să definească trei strategii:
%
%   - alegerea unei perechi de celule neocupate în care să plaseze
%     următoarea piesă (în etapa de plasare a pieselor)
%
%        place(+State, +Color, +Step, +ValidPairs, -Pair)
%
%   - alegerea unei mutări
%
%        move(+State, +Color, +Step, +ValidMoves, -Move)
%
%   - observarea unei piese într-una din cele două poziții posibile
%
%        measure(+State, +Color, +Step, +Piece, -Cell)
%
%   În toate cele trei cazuri, State reprezintă starea curentă a
%   jocului, Color culoarea jucătorului curent, iar Step numărul
%   mutării (important deoarece jocul se termină după maximum 50 de
%   mutări).
%
%
% Mai jos este descris un jucător cu strategii aleatoare.

rand_place(_State, _Color, _Step, ValidPairs, (Cell1, Cell2)):-
    random_member((Cell1, Cell2), ValidPairs).


rand_measure(_State, _Color, _Step, Piece, Cell):-
    Piece = quantum(Cell1, Cell2, _LastColor),
    random_member(Cell, [Cell1, Cell2]), !.

rand_move(_State, _Color, _Step, ValidMoves, Move):-
    random_member(Move, ValidMoves), !.

% ----------------------------------------------------------------------

% [Cerința 8] Definiți strategiile pentru un jucător care să câștige în
% medie mai mult de 50% dintre jocur împotriva jucătorul random.

myValid(A):- A = [[(pos(1,1),pos(1,2)), (pos(1,1),pos(1,3)),(pos(1,2),pos(1,3))],
                  [(pos(2,1),pos(2,2)), (pos(2,1),pos(2,3)),(pos(2,2),pos(2,3))],
                  [(pos(3,1),pos(3,2)), (pos(3,1),pos(3,3)),(pos(3,2),pos(3,3))],
                  [(pos(1,1),pos(2,1)), (pos(1,1),pos(3,1)),(pos(2,1),pos(3,1))],
                  [(pos(1,2),pos(2,2)), (pos(1,2),pos(3,2)),(pos(2,2),pos(3,2))],
                  [(pos(1,3),pos(2,3)), (pos(1,3),pos(3,3)),(pos(2,3),pos(3,3))],
                  [(pos(1,1),pos(2,2)), (pos(1,1),pos(3,3)),(pos(2,2),pos(3,3))],
                  [(pos(1,3),pos(2,2)), (pos(1,3),pos(3,1)),(pos(2,2),pos(3,1))]].




getPosition1([quantum(A,_B,_)],A).
getPosition2([quantum(_A,B,_)],B).

getPositionF1([quantum(A,_B,_)|_],A).
getPositionF2([quantum(_A,B,_)|_],B).


smart_place2(State, Color, Step, ValidPairs, Pair):-
    rand_place(State, Color, Step, ValidPairs, Pair).

smart_place3(State, Color, Step, ValidPairs, Pair):-myValid(Valid),
      findall(X, (member(quantum(A,B,Color),State), X = quantum(A,B,Color)),L),format('Lista este ~p ~n',[L]),
      ( length(L,Lung), Lung == 0,member((P1,P2),Valid),member((P1,P2),ValidPairs),Pair = (P1,P2);
        length(L,Lung), Lung == 1, getPosition1(L,P1),getPosition2(L,P2),
        ( random_member((P1,PP),Valid), member((P1,PP),ValidPairs) , Pair = (P1,PP),format('hei');
          random_member((P2,PP),Valid), member((P2,PP),ValidPairs), Pair = (P2,PP) ,!,format('hei2')) ;
        length(L,Lung) , Lung == 2,  getPositionF1(L,P1),getPositionF2(L,P2),
        ( random_member((P1,PP),Valid), member((P1,PP),ValidPairs) , Pair = (P1,PP);
          member((P2,PP),Valid), member((P2,PP),ValidPairs), Pair = (P2,PP)) ),format(' Runda este  ~d!~n', [Step] ),!.

smart_place4(State,Color,Step,ValidPairs,Pair):-myValid(Valid),
    findall(X, (member(quantum(A,B,Color),State), X = quantum(A,B,Color)),L),length(L,Lung),length(State,Lung2),
    (Lung == 0 , member(Sub1,Valid),member((Rez1,Rez2),Sub1),Pair = (Rez1,Rez2),member(Pair,ValidPairs),!;
     Lung == 1,  member(Sub2,Valid), L = [quantum(P1,P2,_ )] , (member((P1,P2),Sub2),member(Pair,Sub2),member(Pair,ValidPairs),!; member(Pair,Sub2),member(Pair,ValidPairs),!),!;
     Lung == 2,   L = [quantum(P1,P2,_ ),quantum(P11,P22,_)], member(Sub2,Valid),
     ( member((P1,P2),Sub2),member((P11,P22),Sub2),member(Pair,Sub2) ,member(Pair,ValidPairs),!;
       member((P1,P2),Sub2),member(Pair,Sub2) ,member(Pair,ValidPairs),!;
       member((P11,P22),Sub2),member(Pair,Sub2),member(Pair,ValidPairs),!;
       random_member(Pair,ValidPairs),!

     )
    ),!  .


smart_place(State,Color,Step,ValidPairs,Pair):- (member((P1,P2),ValidPairs), State2 = [quantum(P1,P2,Color)|State],Piesa = quantum(P1,P2,Color),
                          has_cycle(State2), (collapse(State2,Piesa,P1,Rez1), winner2(Rez1,[black]),collapse(State2,Piesa,P2,Rez2), winner(Rez2,[black]),!) ,!,Pair = (P1,P2),!;

                                                 smart_place4(State,Color,Step,ValidPairs,Pair)  ).

smart_measure2(State, Color, Step, Piece, Cell):-
    rand_measure(State, Color, Step, Piece, Cell).

smart_measure(State,_, _, Piece, Cell):- Piece = quantum(P1,P2,_),
    (   collapse(State,Piece,P1,New1), winner2(New1,[black]),Cell = P1,! ;
        collapse(State,Piece,P2,New2), winner2(New2,[black]),Cell = P2,! ;
        collapse(State,Piece,P2,New3), winner2(New3,[white]),Cell = P1,! ;
        collapse(State,Piece,P1,New4), winner2(New4,[white]),Cell = P2,! ;
        random_member(Cell,[P1,P1]),!).
smart_move2(State, Color, Step, ValidMoves, Move):-
    rand_move(State, Color, Step, ValidMoves, Move).

smart_move(State, Color, Step, ValidMoves, Move):-
     (    member((Old,New),ValidMoves), delete(State,Old,State2), State3 = [New | State2], has_cycle(State3),New = quantum(P1,P2,_),
          (collapse(State3,New,P1,Rez1), winner2(Rez1 ,[black]),collapse(State3,New,P2,Rez2), winner2(Rez2 ,[black]),! ),!,Move = (Old,New)),!
     ; member((Old,New),ValidMoves), delete(State,Old,State2), State3 = [New | State2],\+ has_cycle(State3),New = quantum(P1,P2,_),!
     ;rand_move(State, Color, Step, ValidMoves, Move).

% ----------------------------------------------------------------------

% [Bonus]. Definiți strategiile pentru un jucător care să câștige în
% medie mai mult de 95% dintre jocuri împotriva jucătorul random.


bonus_place(State, Color, Step, ValidPairs, Pair):-
    smart_place(State, Color, Step, ValidPairs, Pair).

bonus_measure(State, Color, Step, Piece, Cell):-
    smart_measure(State, Color, Step, Piece, Cell).

bonus_move(State, Color, Step, ValidMoves, Move):-
    smart_move(State, Color, Step, ValidMoves, Move).

% ----------------------------------------------------------------------
% ----------------------------------------------------------------------

play2(Player1, Player2, State, Color, Step, LastPiece, Winner):-
    Player1 = (PPlace, PMeasure, PMove),
    ( has_cycle(State), !,
      call(PMeasure, State, Color, Step, LastPiece, Cell),length(State,Lung) ,format(' Runda Mesaeure1 ste  ~p-~p-~p~n', [State,LastPiece,Cell] ),
      collapse(State, LastPiece, Cell, NoCycle),format(' Runda Mesaeure1 ste  ~p-~p-~p~n', [NoCycle,LastPiece,Cell] ), !
    ; NoCycle = State ),
    ( winner(NoCycle, Winner), !
    ; Step =:= 50, !, Winner = [white, black]
    ; ( length(NoCycle, 6), !, valid_moves(NoCycle, Color, ValidMoves),format(' Runda este  ~d!~n', [1] ),
	call(PMove, NoCycle, Color, Step, ValidMoves, Move),
	Move = move(OldPiece, NewPiece),format(' Runda este  ~d!~n', [3] ),
	select(OldPiece, NoCycle, NewPiece, NextState),format(' Runda este  ~d!~n', [4] ), !
      ; valid_pairs(NoCycle, Color, ValidPairs),length(NoCycle,Lung),format(Lung),
	call(PPlace, NoCycle, Color, Step, ValidPairs, (Cell1, Cell2)),format(' Runda este  ~s!~n', [Color] ),
	NewPiece = quantum(Cell1, Cell2, Color),
	NextState = [NewPiece | NoCycle], !),
      next_player(Color, NextColor), Step1 is Step + 1, !,
      play(Player2, Player1, NextState, NextColor, Step1, NewPiece, Winner)
    ).


play(Player1, Player2, State, Color, Step, LastPiece, Winner):-
    Player1 = (PPlace, PMeasure, PMove),
    ( has_cycle(State), !,
      call(PMeasure, State, Color, Step, LastPiece, Cell),
      collapse(State, LastPiece, Cell, NoCycle), !
    ; NoCycle = State ),
    ( winner(NoCycle, Winner), !
    ; Step =:= 500, !, Winner = [white, black]
    ; ( length(NoCycle, 6), !, valid_moves(NoCycle, Color, ValidMoves),
	call(PMove, NoCycle, Color, Step, ValidMoves, Move),
	Move = move(OldPiece, NewPiece),
	select(OldPiece, NoCycle, NewPiece, NextState), !
      ; valid_pairs(NoCycle, Color, ValidPairs),
	call(PPlace, NoCycle, Color, Step, ValidPairs, (Cell1, Cell2)),
	NewPiece = quantum(Cell1, Cell2, Color),
	NextState = [NewPiece | NoCycle], !),
      next_player(Color, NextColor), Step1 is Step + 1, !,
      play(Player2, Player1, NextState, NextColor, Step1, NewPiece, Winner)
    ).
play_against_random(Strategy, Winner):-
    %% Player is black, Rand is white
    Player = (Strategy, black),
    Random = ((rand_place, rand_measure, rand_move), white),
    random_permutation([Player, Random], [(Player1, Color1),(Player2, _)]),
    play(Player1, Player2, [], Color1, 0, none, Winner).


score_against_random(Strategy, Score):-
    score_against_random(Strategy, 1000, 0, 0, 0, WinsNo, DrawsNo, LosesNo),
    format(' Black: ~d, Draws: ~d, White: ~d. ', [WinsNo, DrawsNo, LosesNo]),
    Score is WinsNo / 1000.0.

score_against_random(_, 0, B, D, W, B, D, W):- !.
score_against_random(Strategy, N1, B1, D1, W1, B, D, W):-
    play_against_random(Strategy, Winner),
    (Winner = [black] -> B2 is B1 + 1 ; B2 = B1),
    (Winner = [white] -> W2 is W1 + 1 ; W2 = W1),
    (Winner = [_, _] -> D2 is D1 + 1 ; D2 = D1),
    N2 is N1 - 1,
    score_against_random(Strategy, N2, B2, D2, W2, B, D, W).
