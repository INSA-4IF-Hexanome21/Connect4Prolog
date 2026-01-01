:- consult('ai_V2.pl').
:- consult('ai_random.pl').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% ---------------   CAS DE TEST   --------------------- 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%Test de l'IA random
testAiV1() :- 
    % Création du tableau
    assert(column(1,['r','r','r','j','r','r'],6)),
    assert(column(2,['j','r','r','r','j','r'],6)),
    assert(column(3,['r','r','r','j','r','j'],6)),
    assert(column(4,['j','j','j','r','j','j'],6)),
    assert(column(5,['j','r','r','j','r','r'],6)),
    assert(column(6,['e','e','e','e','e','e'],0)),
    assert(column(7,['j','r','r','j','r','j'],6)),
    aiV1(Move,_),
    Move == 6.

%Test de l'IA V2
testAiV2() :- 
    testAiV2Col(),
    testAiV2Row(),
    testAiV2Row2(),
    testAiV2Diag(),
    testAiV2Diag2(),
    testAiV2Diag3(),
    testAiV2Diag4().

%Vérifie que si 3 jetons sont alignés en colonne alors l'IA va jouer ce coup
testAiV2Col() :- 
    retractall(column(_,_,_)),
    % Création du tableau
    assert(column(1,['r','r','r','e','e','e'],3)),
    assert(column(2,['j','r','r','r','j','r'],6)),
    assert(column(3,['r','r','r','j','r','j'],6)),
    assert(column(4,['j','j','j','r','j','j'],6)),
    assert(column(5,['j','r','r','j','r','r'],6)),
    assert(column(6,['e','e','e','e','e','e'],0)),
    assert(column(7,['j','r','r','j','r','j'],6)),
    aiV2(Move,_),
    Move == 1.

%Vérifie que si 3 jetons sont alignés en ligne alors l'IA va jouer ce coup (dans le cas où le jeton à placer est en fin de ligne)
testAiV2Row() :- 
    retractall(column(_,_,_)),
    % Création du tableau
    assert(column(1,['r','r','r','j','e','e'],4)),
    assert(column(2,['j','r','j','r','j','r'],6)),
    assert(column(3,['r','r','j','j','r','j'],6)),
    assert(column(4,['r','j','r','e','e','e'],3)),
    assert(column(5,['r','r','r','j','j','r'],6)),
    assert(column(6,['e','e','e','e','e','e'],0)),
    assert(column(7,['j','r','r','j','r','j'],6)),
    aiV2(Move,_),
    Move == 6.

%Vérifie que si 3 jetons sont alignés en ligne alors l'IA va jouer ce coup (dans le cas où le jeton à placer est au centre de la ligne)
testAiV2Row2() :- 
    retractall(column(_,_,_)),
    % Création du tableau
    assert(column(1,['r','r','r','j','e','e'],4)),
    assert(column(2,['j','r','j','r','j','r'],6)),
    assert(column(3,['j','r','j','j','r','j'],6)),
    assert(column(4,['r','j','r','e','e','e'],3)),
    assert(column(5,['r','r','r','j','j','r'],6)),
    assert(column(6,['e','e','e','e','e','e'],0)),
    assert(column(7,['r','r','r','j','r','j'],6)),
    aiV2(Move,_),
    Move == 6.

%Vérifie que si 3 jetons sont alignés en diagonale alors l'IA va jouer ce coup (dans le cas où le jeton à placer est en fin de diagonnale)
testAiV2Diag() :- 
    retractall(column(_,_,_)),
    % Création du tableau
    assert(column(1,['r','r','r','j','e','e'],4)),
    assert(column(2,['j','r','j','r','j','r'],6)),
    assert(column(3,['r','r','r','j','r','j'],6)),
    assert(column(4,['j','j','r','e','e','e'],3)),
    assert(column(5,['j','r','r','j','j','r'],6)),
    assert(column(6,['e','e','e','e','e','e'],0)),
    assert(column(7,['r','r','r','j','r','j'],6)),
    aiV2(Move,_),
    Move == 4.

%Vérifie que si 3 jetons sont alignés en diagonnale alors l'IA va jouer ce coup (dans le cas où le jeton à placer est en fin de diagonnale)
%La diagonnale ici étant dans l'autre direction
testAiV2Diag2() :- 
    retractall(column(_,_,_)),
    % Création du tableau
    assert(column(1,['r','r','r','j','e','e'],4)),
    assert(column(2,['j','r','j','r','j','r'],6)),
    assert(column(3,['r','r','j','j','r','j'],6)),
    assert(column(4,['j','j','r','e','e','e'],3)),
    assert(column(5,['j','r','r','j','j','r'],6)),
    assert(column(6,['j','r','j','e','e','e'],3)),
    assert(column(7,['r','r','r','j','r','j'],6)),
    aiV2(Move,_),
    Move == 4.

%Vérifie que si 3 jetons sont alignés en diagonnale alors l'IA va jouer ce coup (dans le cas où le jeton à placer est au centre de la diagonnale)
testAiV2Diag3() :- 
    retractall(column(_,_,_)),
    % Création du tableau
    assert(column(1,['r','r','r','j','e','e'],4)),
    assert(column(2,['j','r','j','r','j','r'],6)),
    assert(column(3,['r','r','e','e','e','e'],2)),
    assert(column(4,['j','j','r','r','e','e'],4)),
    assert(column(5,['j','r','r','j','j','r'],6)),
    assert(column(6,['e','e','e','e','e','e'],0)),
    assert(column(7,['r','r','r','j','r','j'],6)),
    aiV2(Move,_),
    Move == 3.

%Vérifie que si 3 jetons sont alignés en diagonnale alors l'IA va jouer ce coup (dans le cas où le jeton à placer est au centre de la diagonnale)
%La diagonnale ici étant dans l'autre direction
testAiV2Diag4() :- 
    retractall(column(_,_,_)),
    % Création du tableau
    assert(column(1,['r','r','r','j','e','e'],4)),
    assert(column(2,['j','r','j','r','j','r'],6)),
    assert(column(3,['r','r','j','r','e','e'],2)),
    assert(column(4,['j','j','e','e','e','e'],2)),
    assert(column(5,['j','r','r','j','j','r'],6)),
    assert(column(6,['r','e','e','e','e','e'],0)),
    assert(column(7,['r','r','r','j','r','j'],6)),
    aiV2(Move,_),
    Move == 4.