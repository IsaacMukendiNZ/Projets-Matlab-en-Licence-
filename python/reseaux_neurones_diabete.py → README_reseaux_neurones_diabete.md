# Expérimentations de réseaux de neurones pour la prédiction du diabète

## Description
Ce script explore différentes architectures de réseaux de neurones avec TensorFlow/Keras sur deux jeux de données liés au diabète :
1. **Données synthétiques** : BMI (indice de masse corporelle) et diagnostic binaire (diabète ou non).
2. **Dataset diabetes de scikit-learn** : 10 caractéristiques médicales pour une tâche de régression (ou classification selon le code).

Il illustre l’influence du nombre de couches, du learning rate, et du type de perte (binary crossentropy, MSE).

## Prérequis
- Python 3 avec `tensorflow`, `numpy`, `matplotlib`, `scikit-learn`
- Les données sont incluses dans scikit-learn ou générées.

## Utilisation
Exécutez le script :
```bash
python reseaux_neurones_diabete.py
