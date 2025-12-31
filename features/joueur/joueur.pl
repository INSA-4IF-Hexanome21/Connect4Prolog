% Fonction pour changer le joueur en cours
nextPlayer :-
    currentPlayer(Current),
    (   playerJ(ColorJ, TypeJ), Current = player(ColorJ, TypeJ) -> 
        playerR(ColorR, TypeR),
        NextPlayer = player(ColorR, TypeR)
    ; 
        playerJ(ColorJ, TypeJ),
        NextPlayer = player(ColorJ, TypeJ)
    ),
    retract(currentPlayer(Current)),
    assert(currentPlayer(NextPlayer)).
