
---

### **2. Pour `analyse_diabete_pima.py` – `README_analyse_diabete_pima.md`**
```markdown
# Analyse exploratoire du dataset Diabetes (Pima Indians)

## Description
Ce script charge le fichier `diabetes.csv` (Pima Indians Diabetes Database) et effectue une analyse statistique de base :
- Nombre total de patientes
- Répartition saines / diabétiques
- Âge moyen
- Extrêmes du nombre de grossesses
- Détection des valeurs manquantes (codées par 0 dans certaines colonnes)
- Nettoyage et comparaison du taux de glucose entre saines et diabétiques, avec affichage d’un histogramme.

## Prérequis
- Python 3 avec `pandas`, `numpy`, `matplotlib`
- Fichier `diabetes.csv` (téléchargeable par exemple sur Kaggle)

## Utilisation
1. Placez `diabetes.csv` dans le dossier du script.
2. Lancez le script :
   ```bash
   python analyse_diabete_pima.py
