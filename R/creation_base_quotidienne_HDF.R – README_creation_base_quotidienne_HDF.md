# Création d'une base de données quotidienne simulée pour les Hauts-de-France

## Description
Ce script importe trois fichiers Excel (effectifs naissances, taux de mortinatalité, taux de prématurité) pour la région Hauts-de-France. Il calcule les effectifs annuels de mortinatalité et de prématurés, puis génère une base de données quotidienne simulée (2019-2023) avec une distribution aléatoire des événements et l'ajout de vagues de chaleur fictives.

## Prérequis
- R avec les packages : `readxl`, `writexl`, `ggplot2`, `gridExtra`
- Trois fichiers Excel (structure spécifique, voir le script pour les indices de colonnes)

## Utilisation
1. Lancez le script.
2. Sélectionnez les trois fichiers dans l'ordre demandé.
3. La base journalière est sauvegardée sous `Base_HDF_Journaliere.xlsx`.

## Résultats attendus
- Un fichier Excel avec colonnes : date, année, naissances, mortinatalité, naissances prématurées, température moyenne, vague de chaleur.
- Trois graphiques d'évolution annuelle (naissances, mortinatalité, prématurés).

## Remarques
- Les indices de colonnes sont spécifiques au format des fichiers sources ; à adapter si nécessaire.
- La simulation des vagues de chaleur est aléatoire mais reproductible si on fixe la graine.
