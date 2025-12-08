:- consult('fin_de_partie.pl').

clean :-
    retractall(column(_, _, _)).

tests :-
    write('--- TESTS ISOVER ---'),nl,nl,
    test_isOver_0,nl,
    test_isOver_1,nl,
    test_isOver_2,nl,
    test_isOver_3,nl,
    test_isOver_4,nl,
    test_isOver_5,nl,
    test_isOver_6,nl,
    test_isOver_7,nl,
    test_isOver_8,nl.

:- initialization(tests).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% -------------------   TESTS ISOVER   ------------------ 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Test 0: Pas de pièces
test_isOver_0 :-
    clean,
    assert(column(1, [0, 0, 0, 0, 0, 0], 0)),
    assert(column(2, [0, 0, 0, 0, 0, 0], 0)),
    assert(column(3, [0, 0, 0, 0, 0, 0], 0)),
    assert(column(4, [0, 0, 0, 0, 0, 0], 0)),
    assert(column(5, [0, 0, 0, 0, 0, 0], 0)),
    assert(column(6, [0, 0, 0, 0, 0, 0], 0)),
    assert(column(7, [0, 0, 0, 0, 0, 0], 0)),
    (isOver(1, 1) -> write('✗ FAIL: test_isOver_0') ; write('✓ PASS: test_isOver_0')),
    nl,
    clean.

% Test 1: Victoire horizontale pour joueur 1
test_isOver_1 :-
    clean,
    assert(column(1, [1, 0, 0, 0, 0, 0], 1)),
    assert(column(2, [1, 0, 0, 0, 0, 0], 1)),
    assert(column(3, [1, 0, 0, 0, 0, 0], 1)),
    assert(column(4, [1, 0, 0, 0, 0, 0], 1)),
    assert(column(5, [0, 0, 0, 0, 0, 0], 0)),
    assert(column(6, [0, 0, 0, 0, 0, 0], 0)),
    assert(column(7, [0, 0, 0, 0, 0, 0], 0)),
    (isOver(1, 1) -> write('✓ PASS: test_isOver_1') ; write('✗ FAIL: test_isOver_1')),
    nl,
    clean.

% Test 2: Victoire verticale pour joueur 2
test_isOver_2 :-
    clean,
    assert(column(1, [2, 2, 2, 2, 0, 0], 4)),
    assert(column(2, [0, 0, 0, 0, 0, 0], 0)),
    assert(column(3, [0, 0, 0, 0, 0, 0], 0)),
    assert(column(4, [0, 0, 0, 0, 0, 0], 0)),
    assert(column(5, [0, 0, 0, 0, 0, 0], 0)),
    assert(column(6, [0, 0, 0, 0, 0, 0], 0)),
    assert(column(7, [0, 0, 0, 0, 0, 0], 0)),
    (isOver(2, 1) -> write('✓ PASS: test_isOver_2') ; write('✗ FAIL: test_isOver_2')),
    nl,
    clean.

% Test 3: Victoire diagonale droite pour joueur 1
test_isOver_3 :-
    clean,
    assert(column(1, [1, 0, 0, 0, 0, 0], 1)),
    assert(column(2, [0, 1, 0, 0, 0, 0], 2)),
    assert(column(3, [0, 0, 1, 0, 0, 0], 3)),
    assert(column(4, [0, 0, 0, 1, 0, 0], 4)),
    assert(column(5, [0, 0, 0, 0, 0, 0], 0)),
    assert(column(6, [0, 0, 0, 0, 0, 0], 0)),
    assert(column(7, [0, 0, 0, 0, 0, 0], 0)),
    (isOver(1, 2) -> write('✓ PASS: test_isOver_3') ; write('✗ FAIL: test_isOver_3')),
    nl,
    clean.

% Test 4: Victoire diagonale gauche pour joueur 2
test_isOver_4 :-
    clean,
    assert(column(1, [0, 0, 0, 2, 0, 0], 4)),
    assert(column(2, [0, 0, 2, 0, 0, 0], 3)),
    assert(column(3, [0, 2, 0, 0, 0, 0], 2)),
    assert(column(4, [2, 0, 0, 0, 0, 0], 1)),
    assert(column(5, [0, 0, 0, 0, 0, 0], 0)),
    assert(column(6, [0, 0, 0, 0, 0, 0], 0)),
    assert(column(7, [0, 0, 0, 0, 0, 0], 0)),
    (isOver(2, 2) -> write('✓ PASS: test_isOver_4') ; write('✗ FAIL: test_isOver_4')),
    nl,
    clean.

% Test 5: Mauvais joueur vérifié
test_isOver_5 :-
    clean,
    assert(column(1, [1, 0, 0, 0, 0, 0], 1)),
    assert(column(2, [1, 0, 0, 0, 0, 0], 1)),
    assert(column(3, [1, 0, 0, 0, 0, 0], 1)),
    assert(column(4, [1, 0, 0, 0, 0, 0], 1)),
    assert(column(5, [0, 0, 0, 0, 0, 0], 0)),
    assert(column(6, [0, 0, 0, 0, 0, 0], 0)),
    assert(column(7, [0, 0, 0, 0, 0, 0], 0)),
    (isOver(2, 1) -> write('✗ FAIL: test_isOver_5') ; write('✓ PASS: test_isOver_5')),
    nl,
    clean.

% Test 6: Plateau partiellement rempli sans victoire
test_isOver_6 :-
    clean,
    assert(column(1, [2, 1, 2, 1, 1, 0], 5)),
    assert(column(2, [2, 1, 2, 1, 2, 0], 5)),
    assert(column(3, [1, 2, 2, 2, 1, 0], 5)),
    assert(column(4, [1, 2, 1, 2, 1, 0], 5)),
    assert(column(5, [1, 2, 1, 2, 1, 0], 5)),
    assert(column(6, [2, 1, 2, 1, 2, 0], 5)),
    assert(column(7, [1, 2, 2, 1, 1, 0], 5)),
    (isOver(1, 1) -> write('✗ FAIL: test_isOver_6') ; write('✓ PASS: test_isOver_6')),
    nl,
    clean.

% Test 7: Plateau complètement rempli sans victoire
test_isOver_7 :-
    clean,
    assert(column(1, [2, 1, 2, 1, 1, 2], 6)),
    assert(column(2, [2, 1, 2, 1, 2, 1], 6)),
    assert(column(3, [1, 2, 2, 2, 1, 1], 6)),
    assert(column(4, [1, 2, 1, 2, 1, 2], 6)),
    assert(column(5, [1, 2, 1, 2, 1, 2], 6)),
    assert(column(6, [2, 1, 2, 1, 2, 1], 6)),
    assert(column(7, [1, 2, 2, 1, 1, 2], 6)),
    (isOver(1, 1) -> write('✓ PASS: test_isOver_7') ; write('✗ FAIL: test_isOver_7')),
    nl,
    clean.

% Test 8: Plateau complètement rempli avec victoire
test_isOver_8 :-
    clean,
    assert(column(1, [2, 1, 2, 1, 2, 1], 6)),
    assert(column(2, [1, 2, 1, 2, 1, 2], 6)),
    assert(column(3, [2, 1, 2, 1, 2, 1], 6)),
    assert(column(4, [1, 2, 1, 2, 1, 2], 6)),
    assert(column(5, [2, 1, 2, 1, 2, 1], 6)),
    assert(column(6, [1, 2, 1, 2, 1, 2], 6)),
    assert(column(7, [2, 1, 2, 1, 2, 1], 6)),
    (isOver(2, 2) -> write('✓ PASS: test_isOver_8') ; write('✗ FAIL: test_isOver_8')),
    nl,
    clean.