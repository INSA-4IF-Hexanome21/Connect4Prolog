% Trouver Le Meilleur Mouvement
% Faire Un Mouvement Aleatoire
aiV1(Move,_) :- 
    repeat,
    random(1,8,Move),
    column(Move,_,IndexMax),
    not(IndexMax == 6),
    !,
    format(user_error, "AI RANDOM Play : ~w~n", [Move]).