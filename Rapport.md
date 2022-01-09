RAPPORT PROJET PF5
==================

# Indentifiants
 - ROUGEOLLE Yoan @rougeoll 21953845
 - CHERION Jules @cherion 21961980

# Fonctionnalités
 - Convertit la syntaxe concrète polish en syntaxe abstraite. Lorsque la syntaxe concrète n'est pas respectée, affiche un message d'erreur indiquant la ligne où se trouve le problème
 - Réaffiche la syntaxe abstraite en polish
 - Evalue un programme polish. Lorsqu'une variable est non initialisé ou qu'ne opération arithmétique interdite est effectuée, affiche un message d'erreur indiquant la ligne où se trouve le problème
 - Simplification d’un programme polish avec élimination des blocs morts (jamais atteint) et simplification des calculs "évidents"
 - Vérification de l'accés des variables. Indique si une variable est accédée avant son initialisation et donne la liste de toutes les variables utilisées.
 - Donne les signes potentiels et aproximatifs des variables à la fin du programme polish. Indique aussi le risque de division par zéro.
  
# Compilation et Execution

Tout d'abord cloner le dépot git et se déplacer dedans :
```
git clone https://gaufre.informatique.univ-paris-diderot.fr/rougeoll/pf5-projet
cd pf5-projet
```

## Compilation

```
make
```

## Execution
```
./run [option] "Path to a polish program"
```

En tout 5 options :
 - "-reprint" : Réaffiche la syntaxe abstraite en polish
 - "-eval" : Evalue un programme polish
 - "-simpl" : Simplification du programme polish
 - "-vars" : Affichage des variables et le risque d'accés avant initialisation
 - "-sign" : Signes possibles des variables à la fin du programme polish

# Découpage Modulaire

 - polish.ml : le main
 - type.ml : regroupe tous les types neccessaires pour la syntaxe abstraite du polish
 - my_exception.ml : regroupe les exceptions ajoutés par nous, necessaires pour les affichages d'erreurs
 - read.ml : toutes les fonctions necessaires pour transformer la syntaxe concrète en syntaxe abstraite
 - print.ml : toutes les fonctions necessaires pour réafficher la syntaxe abstraite en syntaxe concrète
 - eval.ml : toutes les fonctions necessaires pour l'évaluation d'un programme polish
 - simpl.ml : toutes les fonctions necessaires pour la simplification d'un programme polish
 - vars.ml : toutes les fonctions necessaires pour les vérifications des variables
 - sign.ml : toutes les fonctions necessaires pour l'évaluation des signes des variables

# Organisation du Travail

Yoan a tout d'abord commencé à faire la transformation d'un fichier polish en une liste de ligne associée à leur numéro de ligne. Il s'est ensuite occupé de la lecture des expressions. Jules, quant à lui, s'est occupé de découper la ligne en mots et de compter l'indentation, puis a fait la lecture de condition. Ensuite Yoan a fait un read pour Set, Print et Read. On a tous les deux réfléchis ensemble à comment faire la fonction read_block, puis c'est Jules qui a fait read_block et read_instr. Yoan a fini le read en faisant le read_while et le read_if.
Ensuite on a modifié nos fonctions read et ajouté une exception scpéciale pour le read, pour pouvoir afficher des messages d'erreurs.
Pour le print Yoan a fait une première moitié et Jules a fini le reste.
Pour l'eval Jules a fait une première moitié et Yoan a fini le reste.

Pour le 2e rendu Yoan a commencé par faire le simplification et c'est attaqué à l'analyse des signes.
Jules lui a fait l'analyse des variables et Yoan a ensuite fini l'analyse des signes.


# Divers

Pour le projet nous avons lors de la création des fonctions afin de lire le programme polish fait en sorte que comme pour un compilateur, le programme en cas d'erreur de syntaxe sur le programme polish, on indique le genre d'erreur ainsi que la ligne à laquelle cela se produit. Par exemple si il manque un terme après PPRINT le programme indiquera "Error syntax line **_number of the line_** : Need argument after Print" 

Bien que nous ayons souvent séparer le travail en de gros bloc, surtout vers la fin du projet, nous avons toujours communiqué afin de trouver ce qui nous semble être la meilleur solution.
