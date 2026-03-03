# Ancien script EEG – brouillon pédagogique

## Description
Ce script est une version antérieure et non fonctionnelle d’une analyse EEG. Il contient des erreurs d’indexation (utilisation de `EEG.data(i).latency` au lieu de `EEG.event(i).latency`) et une logique incomplète. Il est conservé à titre d’exemple de ce qu’il ne faut pas faire, pour illustrer les erreurs courantes en programmation MATLAB.

**Ne pas utiliser pour une véritable analyse.** Pour une version fonctionnelle, se référer au script `analyse_eeg_freqtag_erp.m`.

## Prérequis
Aucun (le script ne s’exécute pas correctement).

## Utilisation
Déconseillée. Si vous voulez l’exécuter, corrigez d’abord les erreurs signalées dans les commentaires.

## Remarques
- L’erreur principale est l’accès à `EEG.data(i).latency` alors que les latences sont stockées dans `EEG.event`.
- La boucle `for i=1:10:floor(length(EEG.data)/100)` est également suspecte.
