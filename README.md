# Projet Systèmes Numériques
Dépôt pour le projet du cours de Systèmes Numériques de l'ENS


## Compilation
Pour compiler le simulateur, se placer dans la racine du projet puis effectuer :

        make

## Exécution
L'exécutable du simulateur se trouve dans la racine et se nomme simulator
Les arguments qu'il accepte sont :

        -n i : Effectue la simulation pendant i étapes
        -p : Affiche le programme avant de commencer l'exécution
        -rom file : Fournit la ROM du microprocesseur contenue dans file

## Nettoyage
Pour nettoyer les fichiers compilés, effectuer :

        make clean
