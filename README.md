# Projet Systèmes Numériques
Dépôt pour le projet du cours de Systèmes Numériques de l'ENS :
<https://github.com/Tom-Hubrecht/Projet_Systemes_Numeriques/>


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

## Structure de la ROM
        rom_1
        rom_content_1
        rom_2
        rom_content_2
          .
          .
          .

Où `rom_i` est le nom de la rom dans la netlist et `rom_content_i` est une
suite de {0, 1, t, f}
