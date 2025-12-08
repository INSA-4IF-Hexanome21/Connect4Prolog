nextPlayer(Current, PlayerR, PlayerJ, Next) :-
    Current = PlayerR -> Next = PlayerJ ; Next = PlayerR.