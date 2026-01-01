% IA de plus bas niveau, choisissant une colonne aléatoire à jouer tout en vérifiant si le coup est autorisé
aiV1(Move,_) :- 
    repeat,
    random(1,8,Move),
    column(Move,_,IndexMax),
    not(IndexMax == 6), % Vérifie que la colonne n'est pas pleine
    !,
    format(user_error, "AI RANDOM Play : ~w~n", [Move]).