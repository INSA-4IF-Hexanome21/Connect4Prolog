:- consult('fin_de_partie.pl').

clean :-
    retractall(column(_, _, _)).

tests :-
    write('--- TESTS HORIZONTALVICTORY ---'),nl,nl,
    test_horizontalVictory_0,nl,
    test_horizontalVictory_1,nl,
    test_horizontalVictory_2,nl,
    test_horizontalVictory_3,nl,
    
    write('--- TESTS VERTICALVICTORY ---'),nl,nl,
    test_verticalVictory_0,nl,
    test_verticalVictory_1,nl,
    test_verticalVictory_2,nl,
    test_verticalVictory_3,nl,
    
    write('--- TESTS RIGHTDIAGONALVICTORY ---'),nl,nl,
    test_rightDiagonalVictory_0,nl,
    test_rightDiagonalVictory_1,nl,
    test_rightDiagonalVictory_2,nl,
    test_rightDiagonalVictory_3,nl,
    
    write('--- TESTS LEFTDIAGONALVICTORY ---'),nl,nl,
    test_leftDiagonalVictory_0,nl,
    test_leftDiagonalVictory_1,nl,
    test_leftDiagonalVictory_2,nl,
    test_leftDiagonalVictory_3,nl.

:- initialization(tests).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% -------------   TESTS HORIZONTALVICTORY   ------------- 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Test 0: Pas de pièces
test_horizontalVictory_0 :-
    clean,
    assert(column(1, [0, 0, 0, 0, 0, 0], 0)),
    assert(column(2, [0, 0, 0, 0, 0, 0], 0)),
    assert(column(3, [0, 0, 0, 0, 0, 0], 0)),
    assert(column(4, [0, 0, 0, 0, 0, 0], 0)),
    assert(column(5, [0, 0, 0, 0, 0, 0], 0)),
    assert(column(6, [0, 0, 0, 0, 0, 0], 0)),
    assert(column(7, [0, 0, 0, 0, 0, 0], 0)),
    (horizontalVictory(1, 1) -> write('✗ FAIL: test_horizontalVictory_0') ; write('✓ PASS: test_horizontalVictory_0')),
    nl,
    clean.

% Test 1: 4 pièces successives du même joueur
test_horizontalVictory_1 :-
    clean,
    assert(column(1, [1, 0, 0, 0, 0, 0], 1)),
    assert(column(2, [1, 0, 0, 0, 0, 0], 1)),
    assert(column(3, [1, 0, 0, 0, 0, 0], 1)),
    assert(column(4, [1, 0, 0, 0, 0, 0], 1)),
    assert(column(5, [0, 0, 0, 0, 0, 0], 0)),
    assert(column(6, [0, 0, 0, 0, 0, 0], 0)),
    assert(column(7, [0, 0, 0, 0, 0, 0], 0)),
    (horizontalVictory(1, 1) -> write('✓ PASS: test_horizontalVictory_1') ; write('✗ FAIL: test_horizontalVictory_1')),
    nl,
    clean.

% Test 2: 4 pièces non successives du même joueur
test_horizontalVictory_2 :-
    clean,
    assert(column(1, [1, 0, 0, 0, 0, 0], 1)),
    assert(column(2, [1, 0, 0, 0, 0, 0], 1)),
    assert(column(3, [1, 0, 0, 0, 0, 0], 1)),
    assert(column(4, [0, 0, 0, 0, 0, 0], 1)),
    assert(column(5, [1, 0, 0, 0, 0, 0], 0)),
    assert(column(6, [0, 0, 0, 0, 0, 0], 0)),
    assert(column(7, [0, 0, 0, 0, 0, 0], 0)),
    (horizontalVictory(1, 3) -> write('✗ FAIL: test_horizontalVictory_2') ; write('✓ PASS: test_horizontalVictory_2')),
    nl,
    clean.

% Test 3: 4 pièces successives de joueurs différents
test_horizontalVictory_3 :-
    clean,
    assert(column(1, [1, 0, 0, 0, 0, 0], 1)),
    assert(column(2, [1, 0, 0, 0, 0, 0], 1)),
    assert(column(3, [1, 0, 0, 0, 0, 0], 1)),
    assert(column(4, [2, 0, 0, 0, 0, 0], 1)),
    assert(column(5, [0, 0, 0, 0, 0, 0], 0)),
    assert(column(6, [0, 0, 0, 0, 0, 0], 0)),
    assert(column(7, [0, 0, 0, 0, 0, 0], 0)),
    (horizontalVictory(2, 4) -> write('✗ FAIL: test_horizontalVictory_3') ; write('✓ PASS: test_horizontalVictory_3')),
    nl,
    clean.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% --------------   TESTS VERTICALVICTORY   --------------
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Test 0: Pas de pièces
test_verticalVictory_0 :-
    clean,
    assert(column(1, [0, 0, 0, 0, 0, 0], 0)),
    assert(column(2, [0, 0, 0, 0, 0, 0], 0)),
    assert(column(3, [0, 0, 0, 0, 0, 0], 0)),
    assert(column(4, [0, 0, 0, 0, 0, 0], 0)),
    assert(column(5, [0, 0, 0, 0, 0, 0], 0)),
    assert(column(6, [0, 0, 0, 0, 0, 0], 0)),
    assert(column(7, [0, 0, 0, 0, 0, 0], 0)),
    (verticalVictory(1, 1) -> write('✗ FAIL: test_verticalVictory_0') ; write('✓ PASS: test_verticalVictory_0')),
    nl,
    clean.

% Test 1: 4 pièces successives du même joueur
test_verticalVictory_1 :-
    clean,
    assert(column(1, [1, 1, 1, 1, 0, 0], 4)),
    assert(column(2, [0, 0, 0, 0, 0, 0], 0)),
    assert(column(3, [0, 0, 0, 0, 0, 0], 0)),
    assert(column(4, [0, 0, 0, 0, 0, 0], 0)),
    assert(column(5, [0, 0, 0, 0, 0, 0], 0)),
    assert(column(6, [0, 0, 0, 0, 0, 0], 0)),
    assert(column(7, [0, 0, 0, 0, 0, 0], 0)),
    (verticalVictory(1, 1) -> write('✓ PASS: test_verticalVictory_1') ; write('✗ FAIL: test_verticalVictory_1')),
    nl,
    clean.

% Test 2: 3 pièces successives du même joueur
test_verticalVictory_2 :-
    clean,
    assert(column(1, [1, 1, 1, 0, 0, 0], 5)),
    assert(column(2, [0, 0, 0, 0, 0, 0], 0)),
    assert(column(3, [0, 0, 0, 0, 0, 0], 0)),
    assert(column(4, [0, 0, 0, 0, 0, 0], 0)),
    assert(column(5, [0, 0, 0, 0, 0, 0], 0)),
    assert(column(6, [0, 0, 0, 0, 0, 0], 0)),
    assert(column(7, [0, 0, 0, 0, 0, 0], 0)),
    (verticalVictory(1, 1) -> write('✗ FAIL: test_verticalVictory_2') ; write('✓ PASS: test_verticalVictory_2')),
    nl,
    clean.
    
% Test 3: 4 pièces successives de joueurs différents
test_verticalVictory_3 :-
    clean,
    assert(column(1, [1, 1, 1, 2, 0, 0], 4)),
    assert(column(2, [0, 0, 0, 0, 0, 0], 0)),
    assert(column(3, [0, 0, 0, 0, 0, 0], 0)),
    assert(column(4, [0, 0, 0, 0, 0, 0], 0)),
    assert(column(5, [0, 0, 0, 0, 0, 0], 0)),
    assert(column(6, [0, 0, 0, 0, 0, 0], 0)),
    assert(column(7, [0, 0, 0, 0, 0, 0], 0)),
    (verticalVictory(1, 1) -> write('✗ FAIL: test_verticalVictory_3') ; write('✓ PASS: test_verticalVictory_3')),
    nl,
    clean.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% -----------   TESTS RIGHTDIAGONALVICTORY   ------------
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Test 0: Pas de pièces
test_rightDiagonalVictory_0 :-
    clean,
    assert(column(1, [0, 0, 0, 0, 0, 0], 0)),
    assert(column(2, [0, 0, 0, 0, 0, 0], 0)),
    assert(column(3, [0, 0, 0, 0, 0, 0], 0)),
    assert(column(4, [0, 0, 0, 0, 0, 0], 0)),
    assert(column(5, [0, 0, 0, 0, 0, 0], 0)),
    assert(column(6, [0, 0, 0, 0, 0, 0], 0)),
    assert(column(7, [0, 0, 0, 0, 0, 0], 0)),
    (rightDiagonalVictory(1, 1) -> write('✗ FAIL: test_rightDiagonalVictory_0') ; write('✓ PASS: test_rightDiagonalVictory_0')),
    nl,
    clean.

% Test 1: 4 pièces successives du même joueur en diagonale
test_rightDiagonalVictory_1 :-
    clean,
    assert(column(1, [1, 0, 0, 0, 0, 0], 1)),
    assert(column(2, [0, 1, 0, 0, 0, 0], 2)),
    assert(column(3, [0, 0, 1, 0, 0, 0], 3)),
    assert(column(4, [0, 0, 0, 1, 0, 0], 4)),
    assert(column(5, [0, 0, 0, 0, 0, 0], 0)),
    assert(column(6, [0, 0, 0, 0, 0, 0], 0)),
    assert(column(7, [0, 0, 0, 0, 0, 0], 0)),
    (rightDiagonalVictory(1, 2) -> write('✓ PASS: test_rightDiagonalVictory_1') ; write('✗ FAIL: test_rightDiagonalVictory_1')),
    nl,
    clean.

% Test 2: 4 pièces non successives en diagonale
test_rightDiagonalVictory_2 :-
    clean,
    assert(column(1, [1, 0, 0, 0, 0, 0], 1)),
    assert(column(2, [0, 1, 0, 0, 0, 0], 2)),
    assert(column(3, [0, 0, 0, 0, 0, 0], 3)),
    assert(column(4, [0, 0, 0, 1, 0, 0], 4)),
    assert(column(5, [0, 0, 0, 0, 0, 0], 0)),
    assert(column(6, [0, 0, 0, 0, 0, 0], 0)),
    assert(column(7, [0, 0, 0, 0, 0, 0], 0)),
    (rightDiagonalVictory(1, 4) -> write('✗ FAIL: test_rightDiagonalVictory_2') ; write('✓ PASS: test_rightDiagonalVictory_2')),
    nl,
    clean.

% Test 3: 4 pièces de joueurs différents en diagonale
test_rightDiagonalVictory_3 :-
    clean,
    assert(column(1, [1, 0, 0, 0, 0, 0], 1)),
    assert(column(2, [0, 1, 0, 0, 0, 0], 2)),
    assert(column(3, [0, 0, 1, 0, 0, 0], 3)),
    assert(column(4, [0, 0, 0, 2, 0, 0], 4)),
    assert(column(5, [0, 0, 0, 0, 0, 0], 0)),
    assert(column(6, [0, 0, 0, 0, 0, 0], 0)),
    assert(column(7, [0, 0, 0, 0, 0, 0], 0)),
    (rightDiagonalVictory(1, 3) -> write('✗ FAIL: test_rightDiagonalVictory_3') ; write('✓ PASS: test_rightDiagonalVictory_3')),
    nl,
    clean.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% ------------   TESTS LEFTDIAGONALVICTORY   ------------
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Test 0: Pas de pièces
test_leftDiagonalVictory_0 :-
    clean,
    assert(column(1, [0, 0, 0, 0, 0, 0], 0)),
    assert(column(2, [0, 0, 0, 0, 0, 0], 0)),
    assert(column(3, [0, 0, 0, 0, 0, 0], 0)),
    assert(column(4, [0, 0, 0, 0, 0, 0], 0)),
    assert(column(5, [0, 0, 0, 0, 0, 0], 0)),
    assert(column(6, [0, 0, 0, 0, 0, 0], 0)),
    assert(column(7, [0, 0, 0, 0, 0, 0], 0)),
    (leftDiagonalVictory(1, 1) -> write('✗ FAIL: test_leftDiagonalVictory_0') ; write('✓ PASS: test_leftDiagonalVictory_0')),
    nl,
    clean.

% Test 1: 4 pièces successives du même joueur en diagonale
test_leftDiagonalVictory_1 :-
    clean,
    assert(column(1, [0, 0, 0, 1, 0, 0], 4)),
    assert(column(2, [0, 0, 1, 0, 0, 0], 3)),
    assert(column(3, [0, 1, 0, 0, 0, 0], 2)),
    assert(column(4, [1, 0, 0, 0, 0, 0], 1)),
    assert(column(5, [0, 0, 0, 0, 0, 0], 0)),
    assert(column(6, [0, 0, 0, 0, 0, 0], 0)),
    assert(column(7, [0, 0, 0, 0, 0, 0], 0)),
    (leftDiagonalVictory(1, 2) -> write('✓ PASS: test_leftDiagonalVictory_1') ; write('✗ FAIL: test_leftDiagonalVictory_1')),
    nl,
    clean.

% Test 2: 4 pièces non successives en diagonale
test_leftDiagonalVictory_2 :-
    clean,
    assert(column(1, [0, 0, 0, 1, 0, 0], 4)),
    assert(column(2, [0, 0, 2, 0, 0, 0], 3)),
    assert(column(3, [0, 1, 0, 0, 0, 0], 2)),
    assert(column(4, [1, 0, 0, 0, 0, 0], 1)),
    assert(column(5, [0, 0, 0, 0, 0, 0], 0)),
    assert(column(6, [0, 0, 0, 0, 0, 0], 0)),
    assert(column(7, [0, 0, 0, 0, 0, 0], 0)),
    (leftDiagonalVictory(1, 3) -> write('✗ FAIL: test_leftDiagonalVictory_2') ; write('✓ PASS: test_leftDiagonalVictory_2')),
    nl,
    clean.

% Test 3: 4 pièces de joueurs différents en diagonale
test_leftDiagonalVictory_3 :-
    clean,
    assert(column(1, [0, 0, 0, 2, 0, 0], 4)),
    assert(column(2, [0, 0, 1, 0, 0, 0], 3)),
    assert(column(3, [0, 1, 0, 0, 0, 0], 2)),
    assert(column(4, [1, 0, 0, 0, 0, 0], 1)),
    assert(column(5, [0, 0, 0, 0, 0, 0], 0)),
    assert(column(6, [0, 0, 0, 0, 0, 0], 0)),
    assert(column(7, [0, 0, 0, 0, 0, 0], 0)),
    (leftDiagonalVictory(1, 4) -> write('✗ FAIL: test_leftDiagonalVictory_3') ; write('✓ PASS: test_leftDiagonalVictory_3')),
    nl,
    clean.