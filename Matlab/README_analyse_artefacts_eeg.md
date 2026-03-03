# Détection d'artefacts oculaires dans un signal EEG par corrélation

## Description
Ce script charge deux fichiers MATLAB :
- `Eye_Movement.mat` contenant des motifs oculaires (UP, Down, Blink, Double Blink, Left, Right).
- `EEG_EO.mat` contenant un signal EEG (matrice `EEG_YO`) et sa fréquence d'échantillonnage `Fs`.

Il effectue les opérations suivantes :
1. Affichage des motifs oculaires.
2. Normalisation et filtrage passe-bande (0.5-30 Hz) du signal EEG.
3. Insertion artificielle d'un motif (double blink) sur un canal spécifique (canal 10) à un instant donné.
4. Détection de ce motif dans tous les canaux par corrélation glissante (`xcorr`). Lorsque la corrélation dépasse un seuil, une impulsion est générée.
5. Visualisation du signal EEG modifié et des impulsions de détection.

## Prérequis
- MATLAB avec la Signal Processing Toolbox (pour `butter`, `filtfilt` et `xcorr`).
- Les fichiers de données `Eye_Movement.mat` et `EEG_EO.mat` dans le même dossier que le script.

## Utilisation
1. Placer les fichiers `.mat` dans le dossier du script.
2. Exécuter le script.
3. Observer les graphiques générés.

## Résultats attendus
- Un premier graphique montrant les six motifs oculaires.
- Un second graphique montrant l'EEG filtré sur tous les canaux.
- Un troisième graphique montrant l'EEG après insertion de l'artefact (canal 10 modifié).
- Un dernier graphique montrant les impulsions de détection (un pic là où le template a été trouvé).

## Remarques
- Le seuil de détection est fixé arbitrairement à 50% du max de corrélation. Il peut être ajusté.
- La corrélation est calculée en valeur absolue pour détecter aussi les corrélations négatives.
- Ce script illustre une méthode simple de détection d'artefacts ; en pratique, des techniques plus robustes (ICA, SSP) sont utilisées.
