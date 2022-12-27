# Signification des variables utilisées


Les variables ci-dessous sont des variables calculées par jour. Se reporter aux vignettes du package SIVA 
pour plus de détails sur les calculs. En <span style="color:red"> **rouge** </span> valeurs à éviter, en 
<span style="color:Lime">**vert** </span> valeurs de référence.


|variable               | Définition |
|:----------------------|:-------------------------------------------------------------------------|
|date                   | Date |
|vol_recalcule          | Volume recaculé par le package SIVA à l'aide des niveaux et positions vannes volets |
|debit_moyen_recalcule  | <span style="color:Lime">Débit calculé à partir de la variable précédente</span> |
|vol_bar                | <span style="color:red">Volume total du barrage : attention faux erreur dans le totaliseur de volumne de vannes</span> |
|vol_vanne              | <span style="color:red">Volume calculé à partir du totaliseur du volume de vannes : faux</span> |
|vol_volet              | Volume calculé à partir du totaliseur de volume des volets |
|vol_passe              | Volume calculé pour la passe, ne prends pas en compte le noyage aval |
|vol_siphon             | Volume caclulé pour le siphon |
|vol_ecluse             | Volume calculé pour les débits évacués à l'écluse, les volumes d'eau salée entrants ne sont pas calculés |
|debit_moyen_vol_bar    | <span style="color:red">Débit moyen à partir des volumes du barrage</span> |
|debit_moyen_vol_vanne  | <span style="color:red">Débit moyen à partir des volumes évacués par les vannes </span> |
|debit_moyen_vol_volet  | Débit moyen à partir des volumes évacués par les volets |
|debit_moyen_vol_passe  | Débit moyen à partir des volumes évacués par la passe |
|debit_moyen_vol_siphon | Débit moyen à partir des volumes évacués par le siphon |
|debit_moyen_vol_ecluse | Débit moyen à partir des volumes évacués par l'écluse |
|vol_vanne_barQ         | Volume calculé à partir des débits des vannes 1 à 5 |
|vol_volet_barQ         | Volume calculé à partir des débits des volets 1 à 5 |
|debit_vilaine_estime   | <span style="color:orange">Débit un peu mystérieux estimé à partir du débit du barrage ( ? et des variations du plan d'eau) </span>  |
|debit_moyen_cran       | <span style="color:Lime">Débit moyen journalier de Cran</span> |
|volume_total_barQ      | Volume calculé à partir des débits des vannes et volets 1 à 5, et des volumes siphon, passe et écluse |
|debit_barQ             | <span style="color:Lime">Débit journalier calculé à partir du volume ci-dessus </span> |
|debit_vanne_barQ       | Débit calculé à partir des débits des vannes 1 à 5 |
|debit_volet_barQ       | ébit calculé à partir des débits des volets 1 à 5 |