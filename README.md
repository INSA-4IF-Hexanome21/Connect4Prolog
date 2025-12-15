# Connect4Prolog

## Installation rapide

```bash
git clone "https://github.com/INSA-4IF-Hexanome21/Connect4Prolog.git"
cd Connect4Prolog
```

## Jouer en console

1. Lancer l’interpréteur avec la logique principale :
```bash
swipl puissance4.pl
```
2. Initialiser le plateau et jouer :
```prolog
initBoard.
```
3. À chaque tour, saisissez un numéro de colonne entre 1 et 7. Le jeu s’arrête dès qu’un joueur aligne 4 pions ou quand le plateau est plein.

<!-- ## Interface web (HTTP + front statique)

1. Démarrer le serveur HTTP (port 8080) :
```bash
swipl -s serverl.pl -g start_server
```
2. Ouvrir http://localhost:8080 pour l’interface (fichiers dans [view/](view/index.html)). -->

<!-- ## Tests de performance

… -->

## Tests unitaires

- Fin de partie :
	```bash
	swipl features/fin/tests_fin.pl
	swipl features/fin/tests_victoire.pl
	```
- IA heuristique :
	```bash
	swipl features/ia/testAi.pl
	```
- Pont Negamax :
	```bash
	swipl features/ia/ai.pl
	```