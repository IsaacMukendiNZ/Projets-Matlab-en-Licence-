# Segmentation d’image par couleur simple (pièces de monnaie)

## Description
Ce script segmente une image de pièces de monnaie (`coins1.JPG`) en fonction de leur couleur dominante (rouge, vert, bleu). Pour chaque couleur, un masque binaire est créé à partir de seuils sur les canaux RGB, puis appliqué à l’image originale pour ne conserver que les pièces de cette couleur. Les résultats sont affichés.

## Prérequis
- MATLAB avec Image Processing Toolbox.
- Fichier image `coins1.JPG` dans le dossier.

## Utilisation
1. Placer l’image dans le dossier.
2. Exécuter le script.
3. Observer les masques et les images segmentées pour chaque couleur.

## Résultats attendus
- Quatre figures : une par couleur (masque et segmentation) et une comparaison finale.
- Les seuils sont fixés empiriquement ; ils peuvent être ajustés selon l’image.

## Remarques
- Cette version est plus simple que `segmentation_avancee.m` ; elle n’utilise pas d’opérations morphologiques.
- Les seuils sont basés sur des valeurs fixes ; une approche adaptative serait préférable pour des images variables.
