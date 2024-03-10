# Projet arbre de décision detection de fraude
# UE : Machine Learning, Mr. Pasquier
# Auteurs : Arman Hakobyan et Essotolom N'Gnama
# Formation : Master 2 MIAGE parcours SIRIS

# --------- 0. Mise en place de l'environnement de travail ---------

setwd("C:/Users/LEGION/Desktop/projet_fraudes")
options(scipen = 999)



# --------- 1. Exploration et visualisation des données ---------

# Chargement des données d'étude dans un data frame "declarations_etude"
declarations_etude <- read.csv("donnees_detude.csv", header = TRUE, sep = ",", dec = ".", stringsAsFactors = TRUE)

View(declarations_etude)

# Vérification du chargement des données dans le data frame declarations_etude en affichant la liste des variables et leur type
str(declarations_etude)



# --------- 2. Pré-traitement des données ---------

# À présent que les données sont correctement chargés, il faut les pré-traiter
# On retire lles deux premières colonnes car nous ne les estimons pas utiles pour la prédiction
declarations_etude <- declarations_etude[,-(1:2)]
str(declarations_etude)

# TEST -------------- 
# On retire les variables avec une importance de de moins de 15%
#declarations_etude <- subset(declarations_etude, select = -c(gender, incident_cause, days_to_incident, claim_area, total_policy_claims))
# FIN TEST -------------------

# On visualise le data frame pour essayer de détecter des anomalies visuellement (dans un premier temps)
View(declarations_etude)
# On ne constate aucune anomalie aberrantes dans les données

# On vérifie le nombre de données différentes présent dans la colonne incident_cause
table(declarations_etude[["incident_cause"]])
# L'intérêt est de s'assurer que le data frame ne contient pas de valeur identique, mais écrit différemment comme "nom" et "NON" ou "5" et "cinq"
# On fait attention à la casse des valeurs
# On ne constate aucune anomalie dans les données de la colonne "incident_cause"

# On pourrait éffectuer la même analyse avec les autres colonnes en recopiant la même ligne de code mais en changeant le nom de la colonne
# Cependant on va optimiser un peu notre code en utilisant une boucle "pour".

# On vérifie le nombre de données différentes présentes dans chaque colonne
# Colonne qu'on ne veut pas vérifier, on appliquera d'autres vérifications sur ces colonnes
colonnes_a_exclure <- c("age", "claim_amount", "days_to_incident")

for (col in names(declarations_etude)) { # pour chaque colonne dans le data frame "declarations_etude"
  if (!(col %in% colonnes_a_exclure)) {  # si la colonne n'est pas exclue alors...
    cat("Fréquences des valeurs pour la colonne :", col)
    print(table(declarations_etude[[col]]))
    cat("\n")
  }
}
# On ne constate aucune anomalie dans les données de ces colonnes

# On s'assure qu'aucunes valeurs dans la colonne age ne contienne de valeur iréel comme 0 ou 999
if (any(declarations_etude$age >= 99 | declarations_etude$age == 0)) {
  cat("La colonne age du data frame contient des valeurs invalides\n")
} else {
  cat("RAS\n")
}
# On ne constate aucune anomalie dans les données de la colonne "age"

# On s'assure que la colonne age ne conteinne que des valeurs numériques
if (!is.numeric(declarations_etude$age)) {
  cat("La colonne age du data frame contient des valeurs alphanumérique\n")
} else {
  cat("RAS\n")
}
# On ne constate aucune anomalie dans les données de la colonne "age"

# On s'assure que la colonne days_to_incident ne conteinne que des valeurs numériques
if (!is.numeric(declarations_etude$days_to_incident)) {
  cat("La colonne days_to_incident du data frame contient des valeurs alphanumérique\n")
} else {
  cat("RAS\n")
}
# On ne constate aucune anomalie dans les données de la colonne "days_to_incident"

# On s'assure que la colonne claim_amount ne conteinne que des valeurs numériques
if (!is.numeric(declarations_etude$claim_amount)) {
  cat("La colonne claim_amount du data frame contient des valeurs alphanumérique\n")
} else {
  cat("RAS\n")
}
# On ne constate aucune anomalie dans les données de la colonne "claim_amount"



# --------- 3. Définition de la méthode d'évaluation des classifieurs  ---------

# Nous testerons plusieurs algorithme de classifications durant notre analys : rpart, C50 et treeinstall.packages("rpart")
#install.packages("rpart")
library(rpart)
#install.packages("rpart.plot")
library(rpart.plot)
#install.packages("C50")
library(C50)
#install.packages("tree")
library(tree)

# Il est possible d'appliquer des paramètres pour l’apprentissage des arbres de décision rpart, c50 et tree.
# Ces paramètres pour le classificateur Rpart sont split(gini, information) et minbucket(valeur numérique)
# C50 et Tree ont des paramètres similaires mais aux noms différents.
# 
# Prenons Rpart en exemple
# Le coefficient de Gini est une mesure de l'impureté dans un ensemble de données. 
# Il est calculé en faisant la somme des probabilités au carré de chaque classe. 
# Un coefficient de Gini plus bas indique un ensemble de données plus pur.
# 
# Le gain d'information est une mesure de la quantité d'information gagnée en divisant un ensemble de données en fonction d'une caractéristique particulière.
# Il est calculé en comparant l'entropie de l'ensemble de données d'origine à l'entropie des deux sous-ensembles créés. 
# Un gain d'information plus élevé indique que la caractéristique est plus efficace pour diviser les données.
# 
# Le coefficient de Gini est généralement préféré lorsque les classes sont équilibrées, tandis que le gain d'information est généralement privilégié lorsque les classes sont déséquilibrées.

# Vérifions avec la fonction is.pbalanced() du package plm si notre data frame est équilibré.
#install.packages("plm")
library(plm)
equilibre <- TRUE
for (classe in unique(declarations_etude)) { # pour chaque classe dans le data frame "declarations_etude"
  if (!is.pbalanced(declarations_etude, declarations_etude$classe)) {  # si la classe n'est pas équilibré
    cat("La classe :", col, " n'est pas équilibré\n")
    equilibre <- FALSE
  }
}
if (equilibre) {
  cat("Les classes du data frame sont équilibrés")
}
# Puisque les classes du data frame sont équilibrés, il  est dans l'intéré de notre projet, d'utiliser le paramètre split=gini index

# Le paramètre minbucket contrôle le nombre minimum d'observations requis dans un noeud terminal.
# Augmenter la valeur de minbucket permet de créer des arbres plus petits et moins complexes, ce qui peut réduire le risque de surajustement.
# Il est conseillé d'utiliser des valeurs comme 5 ou 10 pour expérimenter avec le paramètre minbucket.
# Nous avons décider d'utiliser la valeur la plus petite, soit 5.

# Afin d'identifier le meilleur classificateur, c'est-à-dire le plus pertinent pour la détection de fraudes, nous évalurons leurs taux de succès/échecs, leurs matrices de confusions, leurs courbes ROC et indices AUC



# --------- 4. Définition des données d'apprentissage et de test  ---------

# Afin de créer nos arbres de décisions, nous utiliserons les données du data frame declarations_etude, puisque ce dernier contient la colonne fraudulent
# Nous reparations les données de la manière suivante : 80% apprentissage, 20% test
nrow(declarations_etude) # Les données d'études contiennes 1100 lignes
nrow(declarations_etude) * 0.2 # Les données de tests seront constitués des 220 dernière ligne et donc les données d'apprentissage seront constitués des 880 premières lignes

declaration_EA <- declarations_etude[1:880,]
declaration_ET <- declarations_etude[881:1100,]
cat("Taille de l'ensemble d'apprentissage : ", nrow(declaration_EA), "\n")
cat("Taille de l'ensemble de test : ", nrow(declaration_ET), "\n")



# --------- 5. Construction et évaluation des classifieurs  ---------
# --------- 5.1.A. Arbre de décision RPART ---------
? rpart()

# On construit notre arbre de décision rpart
rpart_tree <- rpart(fraudulent ~., declaration_EA, control = rpart.control(split = "gini", minbucket = 5))

# On affiche graphiquement l'abre de décision avec la fonction rpart.plot()
plot(rpart_tree)
text(rpart_tree, pretty=0)

# Après avoir essayé différentes valeurs pour le paramètre minbucket, rester à 5 est plus pertinent.

# On applique l'arbre de decision Rpart sur notre ensemble de test
test_rpart_tree <- predict(rpart_tree, declaration_ET, type="class")

declaration_ET$tree1 <- test_rpart_tree

# On calcul le taux de succes
taux_succes_rpart <- nrow(declaration_ET[declaration_ET$fraudulent==declaration_ET$tree1,]) / nrow(declaration_ET)
cat("Taux de succes de l'arbre de décision Rpart sur l'ensemble de test : ", taux_succes_rpart, " %\n")



# --------- 5.1.B. Arbre de décision C50 ---------
? C5.0()

# On construit notre arbre de décision C50
c50_tree <- C5.0(fraudulent ~., declaration_EA, control = C5.0Control(minCases = 10, noGlobalPruning = TRUE))

# On affiche graphiquement l'abre de décision avec la fonction plot()
plot(c50_tree)

# TEST ---------------
# On installe caret pour utiliser la fonction varImp qui retourne le %tage d'imporance des variables d'un data frame pour la classification
#install.packages("caret")
library(caret)

importance <- varImp(c50_tree)
print(importance)
# On remarque que la moitié des variables ne sont pas importantes (valeur = 0%)
# Au cours d'un test où nous avons gardé uniquement les variables avec plus de 15% d'importance
# Le resultat était le suivant :
# - C5.0 est le classificateur le plus performant
# - le taux de succès de C5.0 a augmenté de 2%
# - la matrice de confusion, la courbe ROC et l'indice AUC ont diminués de 4-5%
# FIN TEST -------------------

# Après avoir essayé différentes valeurs pour le paramètre minbucket, rester à 5 est plus pertinent.

# On applique l'arbre de decision C50 sur notre ensemble de test
test_c50_tree <- predict(c50_tree, declaration_ET, type="class")

declaration_ET$tree2 <- test_c50_tree

# On calcul le taux de succes
taux_succes_c50 <- nrow(declaration_ET[declaration_ET$fraudulent==declaration_ET$tree2,]) / nrow(declaration_ET)
cat("Taux de succes de l'arbre de décision C5.0 sur l'ensemble de test : ", taux_succes_c50, " %\n")



# --------- 5.1.C. Arbre de décision Tree ---------
? tree()

# On construit notre arbre de décision Tree
tree_tree <- tree(fraudulent ~., declaration_EA, split = "gini", control= tree.control(nrow(declaration_EA), mincut = 5))

# On affiche graphiquement l'abre de décision avec la fonction plot()
plot(tree_tree)
text(tree_tree, pretty=0)

# Après avoir essayé différentes valeurs pour le paramètre minbucket, rester à 5 est plus pertinent.

# On applique l'arbre de decision Tree sur notre ensemble de test
test_tree_tree <- predict(tree_tree, declaration_ET, type="class")

declaration_ET$tree3 <- test_tree_tree

# On calcul le taux de succes
taux_succes_tree <- nrow(declaration_ET[declaration_ET$fraudulent==declaration_ET$tree3,]) / nrow(declaration_ET)
cat("Taux de succes de l'arbre de décision Tree sur l'ensemble de test : ", taux_succes_tree, " %\n")



# Comparaison des repartitions des predictions
table(test_rpart_tree)
table(test_c50_tree)
table(test_tree_tree)
# On constate que nos trois arbres de décisions rpart, c50 et tree prédise en moyenne 30 déclarations comme frauduleuses
# Le classificateur Tree, est celui qui prédit le plus de déclarations comme frauduleuses, mais il se peut qu'il s'agisse de faux positifs, nous vérifierons cela prochainement.

# Classement des classificateurs selon le taux de succès :
# N°1 : C5.0 avec 0.7818182  %
# N°2 : RPART avec 0.7454545  %
# N°3 : Tree avec 0.7136364  %
#
# On pourrait arrêter là notre évaluation des classificateurs, et déclarer C5.0 comme le meilleur classificateur.
# Mais, continuons tout de même notre évaluation afin d'être le plus sûr possible.



# --------- 5.2.A. Matrice de Confusion RPART ---------

# Matrice de confusion pour RPART
matrice_rpart <- table(declaration_ET$fraudulent, test_rpart_tree)
print(matrice_rpart) 
# Dans ce tableau nous pouvons constater la répartitions des VP, VN, FP et FN.
# Vrais Positifs (VP) => matrice_rpart[2,2]
# Vrais Negatifs (VN) => matrice_rpart[1,1]
# Faux Positifs (FP) => matrice_rpart[1,2]
# Faux Negatifs (FN) => matrice_rpart[2,1]

# Calcul du rappel (VP / (VP+FN)) soit la capacité du classifieur à détecter les exemples positifs de l'ensemble de test.
matrice_rpart[2,2]/(matrice_rpart[2,2]+matrice_rpart[2,1])

# Calcul de la spécificité (VN / (VN+FP)) soit la capacité du classifieur à détecter les exemples négatifs de l'ensemble de test.
matrice_rpart[1,1]/(matrice_rpart[1,1]+matrice_rpart[1,2])

# Calcul de la précision (VP / (VP+FP)) soit la fiabilité des prédictions positives du classifieur.
matrice_rpart[2,2]/(matrice_rpart[2,2]+matrice_rpart[1,2])

# Calcul du taux de vrais négatifs (VN / (VN+FN)) soit la fiabilité des prédictions négatives du classifieur.
matrice_rpart[1,1]/(matrice_rpart[1,1]+matrice_rpart[2,1])

# Nous pouvons constater suite à ces calculs :
#
# La capacité du classificateur Rpart à détecter les exemples positifs de l'ensemble de test est de 0.2075472 %
# La capacité du classificateur Rpart à détecter les exemples négatifs de l'ensemble de test est de 0.9161677 %
# La fiabilité des prédictions positives du classificateur Rpart est de 0.44 %
# La fiabilité des prédictions négatives du classificateur Rpart est de 0.7846154 %



# --------- 5.2.B. Matrice de Confusion C50 ---------

# Matrice de confusion pour C50
matrice_c50 <- table(declaration_ET$fraudulent, test_c50_tree)
print(matrice_c50) # Dans ce tableau nous pouvons constater la répartitions des VP, VN, FP et FN.

# Calcul du rappel (VP / (VP+FN)) soit la capacité du classifieur à détecter les exemples positifs de l'ensemble de test.
matrice_c50[2,2]/(matrice_c50[2,2]+matrice_c50[2,1])

# Calcul de la spécificité (VN / (VN+FP)) soit la capacité du classifieur à détecter les exemples négatifs de l'ensemble de test.
matrice_c50[1,1]/(matrice_c50[1,1]+matrice_c50[1,2])

# Calcul de la précision (VP / (VP+FP)) soit la fiabilité des prédictions positives du classifieur.
matrice_c50[2,2]/(matrice_c50[2,2]+matrice_c50[1,2])

# Calcul du taux de vrais négatifs (VN / (VN+FN)) soit la fiabilité des prédictions négatives du classifieur.
matrice_c50[1,1]/(matrice_c50[1,1]+matrice_c50[2,1])

# Nous pouvons constater suite à ces calculs :
#
# La capacité du classificateur C5.0 à détecter les exemples positifs de l'ensemble de test est de 0.3018868 %
# La capacité du classificateur C5.0 à détecter les exemples négatifs de l'ensemble de test est de 0.9341317 %
# La fiabilité des prédictions positives du classificateur C5.0 est de 0.5925926 %
# La fiabilité des prédictions négatives du classificateur C5.0 est de 0.8082902 %



# --------- 5.2.C. Matrice de Confusion Tree ---------

# Matrice de confusion pour Tree
matrice_tree <- table(declaration_ET$fraudulent, test_tree_tree)
print(matrice_tree) # Dans ce tableau nous pouvons constater la répartitions des VP, VN, FP et FN.

# Calcul du rappel (VP / (VP+FN)) soit la capacité du classifieur à détecter les exemples positifs de l'ensemble de test.
matrice_tree[2,2]/(matrice_tree[2,2]+matrice_tree[2,1])

# Calcul de la spécificité (VN / (VN+FP)) soit la capacité du classifieur à détecter les exemples négatifs de l'ensemble de test.
matrice_tree[1,1]/(matrice_tree[1,1]+matrice_tree[1,2])

# Calcul de la précision (VP / (VP+FP)) soit la fiabilité des prédictions positives du classifieur.
matrice_tree[2,2]/(matrice_tree[2,2]+matrice_tree[1,2])

# Calcul du taux de vrais négatifs (VN / (VN+FN)) soit la fiabilité des prédictions négatives du classifieur.
matrice_tree[1,1]/(matrice_tree[1,1]+matrice_tree[2,1])

# Nous pouvons constater suite à ces calculs :
#
# La capacité du classificateur C5.0 à détecter les exemples positifs de l'ensemble de test est de 0.3018868 % soit la même valeur que pour C5.0
# La capacité du classificateur C5.0 à détecter les exemples négatifs de l'ensemble de test est de 0.8562874 %
# La fiabilité des prédictions positives du classificateur C5.0 est de 0.4 %
# La fiabilité des prédictions négatives du classificateur C5.0 est de 0.7944444 %



# Classement des classificateurs selon leurs matrices de confusions :
#
# Le classificateur avec le meilleur rappel : C5.0 et Tree avec 0.3018868 %
# Le classificateur avec la meilleure spécificité : C5.0 avec 0.9341317 %
# Le classificateur avec la meilleure précision : C5.0 avec 0.5925926 %
# Le classificateur avec le meilleur taux de vrais négatifs : C5.0 avec 0.8082902%
#
# On constate une fois de plus que les resultats du classificateur C5.0 sont supérieurs à ceux des classificateurs Rpart et Tree.
# Mais, continuons tout de même notre évaluation afin d'être le plus sûr possible.



# --------- 5.3. Courbes ROC et Indices AUC ---------

# Nous allons comparer nos classifieurs à l’aide des représentations graphiques de leurs performances par les courbes ROC et leur indicateur AUC associé.
# Notre objectif est d’identifier graphiquement le classifieur le plus performant parmi plusieurs modèles de prédiction.

#install.packages("ROCR")
library(ROCR)

# Génération des probabilites de prediction sur l'ensemble de test
prob_rpart <- predict(rpart_tree, declaration_ET, type="prob")
prob_c50 <- predict(c50_tree, declaration_ET, type="prob")
prob_tree <- predict(tree_tree, declaration_ET, type="vector")
# print(prob_rpart)
# print(prob_c50)
# print(prob_tree)

# Génération des donnees necessaires pour la courbe ROC
roc_pred_rpart <- prediction(prob_rpart[,2], declaration_ET$fraudulent)
roc_pred_c50 <- prediction(prob_c50[,2], declaration_ET$fraudulent)
roc_pred_tree <- prediction(prob_tree[,2], declaration_ET$fraudulent)

roc_perf_rpart <- performance(roc_pred_rpart,"tpr","fpr")
roc_perf_c50 <- performance(roc_pred_c50,"tpr","fpr")
roc_perf_tree <- performance(roc_pred_tree,"tpr","fpr")

plot(roc_perf_rpart, col = "lightblue")
plot(roc_perf_c50, col = "orange", add = TRUE)
plot(roc_perf_tree, col = "green", add = TRUE)

# Nous pouvons constater que le classificateur le plus performant est C5.0
# Mais pourquoi ?
# Sur le graphique nous pouvons constaster que la courbe orange soit C5.0 est la plus proche de l'angle haut gauche
# de plus cette courbe est celle qui montre la plus grande ascension dans le délai le plus court.

# Une fois de plus le classificateur C5.0 est le plus performant et de loin, cependant si on avait plusieurs courbes similaires
# Le moyen d'identifier le classificateur le plus performant aurait été avec le calcul de leurs AUC.

auc_rpart <- performance(roc_pred_rpart, "auc")
str(auc_rpart)
attr(auc_rpart, "y.values")

auc_c50 <- performance(roc_pred_c50, "auc")
str(auc_c50)
attr(auc_c50, "y.values")

auc_tree <- performance(roc_pred_tree, "auc")
str(auc_tree)
attr(auc_tree, "y.values")

text(0.5, 0.5, paste("AUC RPART =", attr(auc_rpart, "y.values")))
text(0.5, 0.4, paste("AUC C5.0 =", attr(auc_c50, "y.values")))
text(0.5, 0.3, paste("AUC Tree =", attr(auc_tree, "y.values")))
legend("bottomright", legend=c("RPART", "C5.0", "Tree"), col=c("lightblue", "orange", "green"), lwd=2)
# Le classificateur avec le meilleur indice de performance AUC est C5.0 avec 0.6792453 %.



# --------- 6. Choix du classifieur le plus performant  ---------

# Sur la base des résultats de nos calculs, nous pouvons affirmer,
# Le classificateur qui c'est montré le performant dans la prediction de déclarations frauduleuses est : C5.0.
# C'est donc ce classificateur que nous utiliserons pour la prédiction des déclarations frauduleuses.



# --------- 7. Application du classifieur aux données à prédire  ---------

# À présent que nous identifier le classificateur que nous alons utiliser, il est temps de l'appliquer aux données à prédire pour identifier les déclarations frauduleuses.

# Chargement des données pour prediction dans un data frame "declarations_a_predire"
declarations_a_predire <- read.csv("donnees_a_predire.csv", header = TRUE, sep = ",", dec = ".", stringsAsFactors = TRUE)

View(declarations_a_predire)

# Vérification du chargement des données dans le data frame declarations_a_predire en affichant la liste des variables et leur type
str(declarations_a_predire)

# On applique l'arbre de decision C50 sur notre ensemble à prédire pour trouver les classes
resultat_classe <- predict(c50_tree, declarations_a_predire, type="class")

table(resultat_classe)
# On constate après application que 15 déclarations sur 200 sont frauduleuses.

# On applique l'arbre de decision C50 sur notre ensemble à prédire pour trouver les probabilités
resultat_prob <- predict(c50_tree, declarations_a_predire, type="prob")

# On ajoute nous prédictions dans deux nouvelles colonnes de l'ensemble à prédire
declarations_a_predire$fraudulent <- resultat_classe
declarations_a_predire$probabilite <- resultat_prob

# Visualisation de l'ensemble à prédire
View(declarations_a_predire)

# Exportons pour finir le résultat de nos prédiction dans un fichier au format .CSV
data_frame <- data.frame(customer_id = declarations_a_predire$customer_id, fraudulent = resultat_classe, probabilite = resultat_prob)

fichier_csv <- "resultats_predictions_hakobyan_ngnama.csv"

? write()

# Ecriture du fichier
write.table(data_frame, fichier_csv, sep=",", dec=".", row.names = FALSE)