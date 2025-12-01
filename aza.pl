placer_jeton(_PlateauActuel, NumeroCol, Joueur, _NouveauPlateau) :-
    num_rows(NbLignes), % en cas qun jour on augmente le nombre de lignes, je lai fait dynamique

    column(NumeroCol, DonneesColAnciennes, DernierePosLibre),
    DernierePosLibre =< NbLignes,
    DernierePosLibre >= 1, 

    replace_nth1_in_list(DernierePosLibre, Joueur, DonneesColAnciennes, NouvellesDonneesCol),

    NouvelleDernierePosLibre is DernierePosLibre + 1,

    retract(column(NumeroCol, DonneesColAnciennes, DernierePosLibre)),
    assertz(column(NumeroCol, NouvellesDonneesCol, NouvelleDernierePosLibre)).


% Remplace l'élément à l'Index (1-basé) dans Liste par NouvelElement pour obtenir NouvelleListe.
replace_nth1_in_list(1, NouvelElement, [_|Queue], [NouvelElement|Queue]) :- !. % Cas de base : premier élément
replace_nth1_in_list(N, NouvelElement, [Tete|Queue], [Tete|Resultat]) :-
    N > 1, % Récursivité : si l'index n'est pas 1
    N1 is N - 1, % On décrémente l'index
    replace_nth1_in_list(N1, NouvelElement, Queue, Resultat).