# Analyse des taux de mortalité par région (écarts à la moyenne)

## Description
Ce script charge deux fichiers Excel (taux de mortalité et effectifs de naissances) pour calculer les écarts régionaux par rapport à la moyenne nationale. Il produit :
- Un graphique d'évolution des taux par région avec la moyenne globale.
- Un diagramme en barres des écarts (excès/déficits) pour la dernière année disponible.

## Prérequis
- R avec les packages : `readxl`, `dplyr`, `ggplot2`
- Deux fichiers Excel structurés (col1: Année, col2: Code région, col3: Région, col4: Type, col5: Taux ou Effectif)

## Utilisation
1. Exécutez le script.
2. Sélectionnez d'abord le fichier des taux de mortalité, puis celui des effectifs.
3. Les résultats s'affichent dans la console et deux graphiques apparaissent.

## Résultats attendus
- Moyenne globale des taux.
- Évolution temporelle avec ligne de référence.
- Barres des écarts pour la dernière année, avec étiquettes.

## Remarques
- Le script filtre automatiquement les lignes "total" pour les taux.
- Les codes DOM sont exclus si nécessaire (à adapter).
