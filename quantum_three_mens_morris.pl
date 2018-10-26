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

% next_player/2
next_player(white, black).
next_player(black, white).


% ----------------------------------------------------------------------

% [Cerința 2] Scrieți un predicat cell(?Cell) care să fie adevărat
% pentru atunci când Cell este o structură pos(X,Y) reprezentând o
% celulă de pe tablă.

% cell/1
cell(pos(X,Y)):- member(X,[1, 2, 3]), member(Y, [1, 2, 3]).


% ----------------------------------------------------------------------

% [Cerința 3] Scrieți un predicat valid_pairs(+State, +Color, -Pairs)
% care să descrie legătura dintre starea State și toate perechile de
% celule în care jucătorul Color ar putea așeza o piesă în stare
% cuantică. Celulele ocupate de piese în stare clasică nu reprezintă
% soluții valide. De asemenea, nici perechile de celule deja legate
% cuantic de o piesă a aceluiași jucător nu reprezintă perchi valide.
% Lista Pairs trebuie să NU conțină și o pereche și inversa ei.


% valid_pairs/3

%cells = lista cu toate pozitiile clasice
invalid_cells(State, Cells):- findall(X, (member(classic(X, _), State), cell(X)), Cells).

%perechile invalide din quantumurile culorii respective
invalid_pairs(State, Color, IPairs):- findall((X,Y),
                                      (member(quantum(X, Y, Color), State), cell(X), cell(Y)),
                                      IPairs).

valid_pairs(State, Color, Pairs):- findall((X, Y),
                                  (cell(X), cell(Y), invalid_cells(State, Cells), \+ member(X, Cells),
                                  \+ member(Y, Cells), invalid_pairs(State, Color, IPairs), \+ member((X,Y), IPairs), X @< Y),
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

%pentru generarea tuturor culorilor
is_Color(black).
is_Color(white).

%miscarile clasice care pot fi efectuate de catre Color
classic_moves(State, Color, CMoves):- findall(move(classic(X, Color), quantum(Y, Z, Color)),
                                      (valid_pairs(State, Color, Pairs),
                                      cell(X), cell(Y), cell(Z),
                                      member((Y,Z), Pairs),
                                      member(classic(X, Color), State), Y \= Z, Y @< Z),
                                      CMoves).

%miscarile quantice care pot fi efectuare de catre Color
quantum_move(State, Color, QMoves):- findall(move(quantum(X, Y, Color), quantum(Z, T, Color)),
                                     (valid_pairs(State, Color, Pairs),
                                     cell(X), cell(Y), cell(Z), cell(T), 
                                     X \= Z, X \= T, Y \= Z, Y \= T,
                                     member((Z, T), Pairs),
                                     member(quantum(X, Y, Color), State), Z @< T),
                                     QMoves).

valid_moves(State, Color, Moves):- findall(M, 
                                   (classic_moves(State, Color, CMoves), quantum_move(State, Color, QMoves),
                                   (member(M, CMoves); member(M, QMoves))),
                                   Moves).

% ----------------------------------------------------------------------

% Cerința 5. Scrieți un predicat winner(+State, -Winner) care produce
% legarea variabilei Winner la lista jucătorilor care au cele trei
% piese în stare clasică aliniate. Dacă nimeni nu a câștigat, winner
% eșuează (nu leagă Winner la lista vidă).

% winner/2

%toate pozitiile care sunt castigatoare
game_set([pos(X, Y), pos(X, Z), pos(X, T)]):- member(X, [1, 2, 3]), member(Y, [1, 2, 3]), member(Z, [1, 2, 3]), member(T, [1, 2, 3]), Y \= Z, Y \= T, Z \= T.
game_set([pos(Y, X), pos(Z, X), pos(T, X)]):- member(X, [1, 2, 3]), member(Y, [1, 2, 3]), member(Z, [1, 2, 3]), member(T, [1, 2, 3]), Y \= Z, Y \= T, Z \= T.
game_set([pos(X, X), pos(Y, Y), pos(Z, Z)]):- member(X, [1, 2, 3]), member(Y, [1, 2, 3]), member(Z, [1, 2, 3]), X \= Y, X \= Z, Y \= Z.
game_set([pos(X, Y), pos(2, 2), pos(Y, X)]):- member(X, [1, 3]), member(Y, [1, 3]), X \= Y.

%toate piesele aflate in stare clasica pe mapa pentru Color
get_classic_cells(State, Color, List):- findall(X, (member(classic(X, Color), State), cell(X)), List).

%length >= 1 pentru a returna fail in caz ca nu e nimeni castigator
winner(State, Colors):- findall(Color, 
                        (is_Color(Color), get_classic_cells(State, Color, List), game_set(List)),
                        Colors), length(Colors, X), X >= 1.

% ----------------------------------------------------------------------

% Cerința 6. Se cere scrierea unui predicat has_cycle(+State) care să
% fie adevărat dacă starea repsectivă conține un ciclu de celule
% legate cuantic.
%
% has_cycle([quantum(pos(1,1), pos(3,2), white),
%            quantum((2,1), (3,2), black),
%            quantum(pos(1,1), pos(2,1), white)])
%   => true.
%
% has_cycle([quantum(pos(1,1), pos(3,2), black),
%            quantum(pos(3,1), pos(3,2), white)])
%   => false.

% has_cycle/1

%am facut pe rand si sunt doar teste de 3 si de 4 aparent (3 sau 4 piese participante in ciclu, se puteau face aceleasi functii si pentru restul de 5 si 6 posibile)
%e adevarat cand e una sau cealalta, urmand ordine crescatoare pentru o complexitate mai mica
has_cycle(S):- cycle_3(S) ; cycle_4(S).

cycle_3(S):- member(quantum(X, Y, _), S), select(quantum(X, Y, _), S, S2), 
            ( (member(quantum(Y, Z, _), S2), select(quantum(Y, Z, _), S2, S3)) ; (member(quantum(Z, Y, _), S2), select(quantum(Z, Y, _), S2, S3)) )
            , (member(quantum(Z, X, _), S3) ; member(quantum(X, Z, _), S3)), !.
cycle_4(S):- member(quantum(X, Y, _), S), select(quantum(X, Y, _), S, S2), 
            ( (member(quantum(Y, Z, _), S2), select(quantum(Y, Z, _), S2, S3)) ; (member(quantum(Z, Y, _), S2), select(quantum(Z, Y, _), S2, S3)) ),
            ( (member(quantum(Z, T, _), S3), select(quantum(Z, T, _), S3, S4)) ; (member(quantum(T, Z, _), S3), select(quantum(T, Z, _), S3, S4)) ),
            (member(quantum(T, X, _), S4) ; member(quantum(X, T, _), S4 )), !.


%TRYHARDS...

%cycle_3(State, X, Y, Z):- (member(quantum(X, Y, _), State), select(quantum(X, Y, _), State, NewState), cycle_3(NewState, Y, Z, X)) ; (length(State, T), T == 0).
%cycle_3(S):- \+ (\+ member(quantum(X, Y, _), S); \+ member(quantum(Y, Z, _), S); \+ member(quantum(Z, X, _), S)).
%cycle_3(S):- \+ (\+ member(quantum(X, Y, _), S); \+ member(quantum(Y, Z, _), S); \+ member(quantum(Z, X, _), S)).
%cycle_4(State, Pair):-findall((X, Y, Z, T), (member(quantum(X, Y, _), State), %nu merge nici daca le pun diferite intre ele
%                ( member(quantum(Y, Z, _), State); member(quantum(Z, Y, _), State)),
%                ( member(quantum(Z, T, _), State); member(quantum(T, Z, _), State)), 
%                ( member(quantum(T, X, _), State); member(quantum(X, T, _), State))),
%                Pair).
%astea de ce nu merg? le pastrez pentru intrebare la corectare


% ----------------------------------------------------------------------

% Cerința 7. Se cere scrierea unui predicat collapse(+State, +Piece,
% +Cell, -NewState) care să producă starea obținută prin "măsurarea"
% piesei Piece în celula Cell. Starea NewState este rezulatul
% colapsării stării State. Piece este garantat un membru al lui State.

% collapse/4
% collapse(+State, +Piece, +Cell, -NewState)


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
    random_member((Cell1, Cell2), ValidPairs), !.

rand_measure(_State, _Color, _Step, Piece, Cell):-
    Piece = quantum(Cell1, Cell2, _LastColor),
    random_member(Cell, [Cell1, Cell2]), !.

rand_move(_State, _Color, _Step, ValidMoves, Move):-
    random_member(Move, ValidMoves), !.

% ----------------------------------------------------------------------

% [Cerința 8] Definiți strategiile pentru un jucător care să câștige în
% medie mai mult de 80% dintre jocur împotriva jucătorul random.


smart_place(State, Color, Step, ValidPairs, Pair):-
    rand_place(State, Color, Step, ValidPairs, Pair).

smart_measure(State, Color, Step, Piece, Cell):-
    rand_measure(State, Color, Step, Piece, Cell).

smart_move(State, Color, Step, ValidMoves, Move):-
    rand_move(State, Color, Step, ValidMoves, Move).

% ----------------------------------------------------------------------

% [Bonus]. Definiți strategiile pentru un jucător care să câștige în
% medie mai mult de 95% dintre jocuri împotriva jucătorul random.


bonus_place(State, Color, Step, ValidPairs, Pair):-
    rand_place(State, Color, Step, ValidPairs, Pair).

bonus_measure(State, Color, Step, Piece, Cell):-
    rand_measure(State, Color, Step, Piece, Cell).

bonus_move(State, Color, Step, ValidMoves, Move):-
    rand_move(State, Color, Step, ValidMoves, Move).

% ----------------------------------------------------------------------
% ----------------------------------------------------------------------

% verbose.  %% Comentați linia aceasta pentru a vedea evoluția jocurilor.
verbose:- fail.

play(Player1, Player2, State, Color, Step, LastPiece, Winner):-
    Player1 = (PPlace, PMeasure, PMove),
    ( verbose -> format('-------------------- Pas [~w]~n', [Step]),
      format('Apel has_cycle(~w)...~n', [State]); true ),
    ( has_cycle(State) ->
      ( verbose -> format('Apel ~w(~w, ~w, ~w, ~w, Cell)...~n',
	       [PMeasure, State, Color, Step, LastPiece]) ; true ),
      ( call(PMeasure, State, Color, Step, LastPiece, Cell) ->
	( verbose -> format('Apel collapse(~w, ~w, ~w, NoCycle)...~n',
		 [State, LastPiece, Cell]); true ),
        ( collapse(State, LastPiece, Cell, NoCycle) ->
	  true
	; format('collapse(~w, ~w, ~w, NoCycle) a eșuat.~n',
		 [State, LastPiece, Cell]),
	  !, fail)
      ; format('~w(~w, ~w, ~w, ~w, Cell) a eșuat.~n',
	       [PMeasure, State, Color, Step, LastPiece]),
	!, fail)
    ; NoCycle = State),
    ( winner(NoCycle, Winner), !
    ; Step =:= 50, !, Winner = [white, black],
      (   verbose -> format('Am ajuns la pasul 50.~n'); true )
    ; ( length(NoCycle, 6) ->
	( verbose -> format('Apel valid_moves(~w, ~w, ValidMoves).~n',
		 [NoCycle, Color]); true ),
        ( valid_moves(NoCycle, Color, ValidMoves)->
	  ( verbose -> format('Apel ~w(~w, ~w, ~w, ~w, Move)...~n',
		   [PMove, NoCycle, Color, Step, ValidMoves]); true ),
          ( call(PMove, NoCycle, Color, Step, ValidMoves, Move) ->
	    Move = move(OldPiece, NewPiece),
	    select(OldPiece, NoCycle, NewPiece, NextState), !
	  ; format('~w(~w, ~w, ~w, ~w, Move) a eșuat.~n',
		   [PMove, NoCycle, Color, Step, ValidMoves]),
	    !, fail)
	; format('valid_moves(~w, ~w, ValidMoves) a eșuat.~n',
		 [NoCycle, Color]),
	  !, fail)
      ; (verbose -> format('Apel valid_pairs(~w, ~w, ValidPairs)...~n',
                               [NoCycle, Color]); true),
        ( valid_pairs(NoCycle, Color, ValidPairs) ->
          ( verbose -> format('Apel ~w(~w, ~w, ~w, ~w, (Cell1, Cell2)).~n',
		   [PPlace, NoCycle, Color, Step, ValidPairs]); true),
	  ( call(PPlace, NoCycle, Color, Step, ValidPairs, (Cell1, Cell2)) ->
	    NewPiece = quantum(Cell1, Cell2, Color),
	    NextState = [NewPiece | NoCycle], !
	  ; format('~w(~w, ~w, ~w, ~w, (Cell1, Cell2)) a eșuat.~n',
		   [PPlace, NoCycle, Color, Step]),
	    !, fail)
	; format('valid_pairs(~w, ~w, ValidPairs) a eșuat.~n', [NoCycle, Color]),
	  !, fail) ),
      next_player(Color, NextColor), Step1 is Step + 1, !,
      play(Player2, Player1, NextState, NextColor, Step1, NewPiece, Winner) ).


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
    ( verbose ->
      format('>>>>>>>>>>> Mai sunt de jucat ~w jocuri. Strategia a câștigat ~w jocuri, Random ~w jocuri, ~w remize. ~n',
	     [N2, B2, W2, D2])
    ; true ),
    score_against_random(Strategy, N2, B2, D2, W2, B, D, W).
