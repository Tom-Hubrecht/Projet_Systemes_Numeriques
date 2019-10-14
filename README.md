# Projet_Systemes_Numeriques
Dépôt pour le projet du cours de Systèmes Numériques de l'ENS


## Compilation
Pour compiler le simulateur, se placer dans la racine du projet puis effectuer :
        make

## Exécution
L'exécutable du simulateur se trouve dans la racine et se nomme simulator
Les arguments qu'il accepte sont :
        -n i : Effectue la simulation pendant i étapes
        -rom file : Fournit la ROM du microprocesseur contenue dans file
        -ram k : Décrit la taille du vecteur d'adressage de la RAM i.e. une taille de 2^k
