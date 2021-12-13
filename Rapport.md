RAPPORT PROJET PF5
==================

# Indentifiants
 - ROUGEOLLE Yoan @rougeoll 21953845
 - CHERION Jules @cherion 

# Fonctionnalités
 - Convertit la syntaxe concrète polish en syntaxe abstraite. Lorsque la syntaxe concrète n'est pas respecté, affiche un message d'erreur indiquant la ligne où se trouve le problème
 - Réaffiche la syntaxe abstraite en polish
 - Evalue un programme polish. Lorsqu'une variable est non initialisé ou que une opération arithmétique interdite est effectué, affiche un message d'erreur indiquant la ligne où se trouve le problème
  
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

En tout 2 options :
 - "-reprint" : Réaffiche la syntaxe abstraite en polish
 - "-eval" : Evalue un programme polish

# Découpage Modulaire

 - polish.ml : le main
 - type.ml : regroupe tous les types neccessaires pour la syntaxe abstraite du polish
 - my_exception.ml : regroupe les exceptions ajoutés par nous, necessaires pour les affichages d'erreurs
 - read.ml : toutes les fonctions necessaires pour transformer la syntaxe concrète en syntaxe abstraite
 - print.ml : toutes les fonctions necessaires pour réafficher la syntaxe abstraite en syntaxe concrète
 - eval.ml : toutes les fonctions necessaires pour l'évaluation d'un programme polish

# Organisation du Travail

Yoan a tout d'abord commencé à faire la transformation d'un fichier polish en une liste de ligne associée à leur numéro de ligne. Il s'est ensuite occupé de la lecture des expressions. Jules, quant à lui, s'est occupé de découper la ligne en mots et de compter l'indentation, puis a fait la lecture de condition. Ensuite Yoan a fait un read pour Set, Print et Read. On a tous les deux réfléchis ensemble à comment faire la fonction read_block, puis c'est Jules qui a fait read_block et read_instr. Yoan a fini le read en faisant le read_while et le read_if.
Ensuite on a modifié nos fonctions read et ajouté une exception scpéciale pour le read, pour pouvoir afficher des messages d'erreurs.
Pour le print Yoan a fait une première moitié et Jules a fini le reste.
Pour l'eval Jules a fait une première moitié et Yoan a fini le reste.