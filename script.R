# Projet arbre de décision detection de fraude
# UE : Machin Learning, Mr. Pasquier
# Auteurs : Arman Hakobyan et Essotolom N'Gnama
# Formation : Master 2 MIAGE parcours SIRIS

# --------- 0. Mise en place de l'environnement de travail ---------

setwd("C:/Users/LEGION/Desktop/projet_fraudes")
options(scipen = 999)



# --------- 1. Exploration et visualisation des données ---------

# Chargement des données d'étude dans un data frame "declarations_etude"
declarations_etude <- read.csv("donnees_detude.csv", header = TRUE, sep = ",", dec = ".", stringsAsFactors = TRUE)
# Chargement des données pour prediction dans un data frame "declarations_a_predire"
declarations_a_predire <- read.csv("donnees_a_predire.csv", header = TRUE, sep = ",", dec = ".", stringsAsFactors = TRUE)

View(declarations_etude)
View(declarations_a_predire)

# Vérification du chargement des données dans le data frame declarations_etude en affichant la liste des variables et leur type
str(declarations_etude)
# Vérification du chargement des données dans le data frame declarations_a_predire en affichant la liste des variables et leur type
str(declarations_a_predire) 
# On remarque que le data frame contenant les déclarations à prédire ne contient pas de colonnes fraudilent



# --------- 2. Pré-traitement des données ---------

# À présent que les données sont correctement chargés, il faut les pré-traiter
# On retire lles deux premières colonnes car nous ne les estimons pas utiles pour la prédiction
declarations_etude <- declarations_etude[,-(1:2)]
str(declarations_etude)
#declarations_a_predire <- declarations_a_predire[,-(1:2)]
#str(declarations_a_predire)

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

# Nous testerons plusieurs algorithme de classifications durant notre analys : rpart, C50 et tree
library(rpart)
library(C50)
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
# Puis que les classes du data frame sont équilibrés, il  est dans l'intéré de notre projet, d'utiliser le paramètre split=gini index

# Le paramètre minbucket contrôle le nombre minimum d'observations requis dans un noeud terminal.
# Augmenter la valeur de minbucket permet de créer des arbres plus petits et moins complexes, ce qui peut réduire le risque de surajustement.
# Il est conseillé d'utiliser des valeurs comme 5 ou 10 pour expérimenter avec le paramètre minbucket.
# Nous avons décider d'utiliser la valeur la plus petite, soit 5.

# Afin d'identifier le meilleur classificateur, c'est-à-dire le plus pertinent pour la détection de fraudes, nous évalurons leurs taux de succès/échecs et leurs matrices de confusions



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
# --------- 5.A. RPART ---------
? rpart()

# On construit notre arbre de décision rpart
rpart_tree <- rpart(fraudulent ~., declaration_EA, control = rpart.control(split = "gini", minbucket = 5))

# On affiche graphiquement l'abre de décision avec la fonction rpart.plot()
plot(rpart_tree)
text(rpart_tree, pretty=0)

# Après avoir essayé différentes valeurs pour le paramètre minbucket, rester à 5 est plus pertinent.

#On applique l'arbre de decision Rpart sur notre ensemble de test
test_rpart_tree <- predict(rpart_tree, declaration_ET, type="class")

declaration_ET$tree1 <- test_rpart_tree

# On calcul le taux de succes
taux_succes_rpart <- nrow(declaration_ET[declaration_ET$fraudulent==declaration_ET$tree1,]) / nrow(declaration_ET)
cat("Taux de succes de l'arbre de décision Rpart sur l'ensemble de test : ", taux_succes_rpart, " %\n")



# --------- 5.B. C50 ---------
? C5.0()

# On construit notre arbre de décision C50
c50_tree <- C5.0(fraudulent ~., declaration_EA, control = C5.0Control(minCases = 10, noGlobalPruning = TRUE))

# On affiche graphiquement l'abre de décision avec la fonction plot()
plot(c50_tree)

# Après avoir essayé différentes valeurs pour le paramètre minbucket, rester à 5 est plus pertinent.

#On applique l'arbre de decision Rpart sur notre ensemble de test
test_c50_tree <- predict(c50_tree, declaration_ET, type="class")

declaration_ET$tree2 <- test_c50_tree

# On calcul le taux de succes
taux_succes_c50 <- nrow(declaration_ET[declaration_ET$fraudulent==declaration_ET$tree2,]) / nrow(declaration_ET)
cat("Taux de succes de l'arbre de décision C5.0 sur l'ensemble de test : ", taux_succes_c50, " %\n")



# --------- 5.C. Tree ---------
? tree()

# On construit notre arbre de décision Tree
tree_tree <- tree(fraudulent ~., declaration_EA, split = "gini", control= tree.control(nrow(declaration_EA), mincut = 5))

# On affiche graphiquement l'abre de décision avec la fonction plot()
plot(tree_tree)
text(tree_tree, pretty=0)

# Après avoir essayé différentes valeurs pour le paramètre minbucket, rester à 5 est plus pertinent.

#On applique l'arbre de decision Rpart sur notre ensemble de test
test_tree_tree <- predict(tree_tree, declaration_ET, type="class")

declaration_ET$tree3 <- test_tree_tree

# On calcul le taux de succes
taux_succes_tree <- nrow(declaration_ET[declaration_ET$fraudulent==declaration_ET$tree3,]) / nrow(declaration_ET)
cat("Taux de succes de l'arbre de décision Tree sur l'ensemble de test : ", taux_succes_tree, " %\n")



# Classement des classificateurs selon le taux de succès :
# N°1 : C5.0 avec 0.7818182  %
# N°2 : RPART avec 0.7454545  %
# N°3 : Tree avec 0.7136364  %
#
# On pourrait arrêter là notre évaluation des classificateurs, et déclarer C5.0 comme le meilleur classificateur.
# Mais, continuons tout de même notre évaluation afin d'être le plus sûr.

# ...