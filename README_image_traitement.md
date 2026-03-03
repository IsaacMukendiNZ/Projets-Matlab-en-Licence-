# Prétraitement d’image pour la reconnaissance de formes (caractères, objets)

## Description
Ce script illustre une chaîne de traitement d’image basique : conversion en niveaux de gris, binarisation, détection de contours, dilatation, remplissage de trous, étiquetage des composantes connexes et encadrement des objets détectés. L’exemple utilise l’image `ETR.jpg` (lettres). Il compte et délimite les objets (ici 3).

## Prérequis
- MATLAB avec la boîte à outils Image Processing.
- Fichier image `ETR.jpg` dans le même dossier.

## Utilisation
1. Placer l’image dans le dossier.
2. Exécuter le script.
3. Observer les étapes intermédiaires et le résultat final avec les rectangles.

## Résultats attendus
- Affichage progressif de l’image transformée.
- Le nombre d’objets détectés est affiché dans la console.
- L’image originale est superposée avec les boîtes englobantes.
