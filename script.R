# 0. Mise en place de l'environnement de travail ---------

setwd("C:/Users/LEGION/Desktop/projet_fraudes")
options(scipen = 999)

# 1. Exploration et visualisation des données ---------

# Chargement des données d'étude dans un data frame "declarations_etude"
declarations_etude <- read.csv("donnees_detude.csv", header = TRUE, sep = ",", dec = ".", stringsAsFactors = TRUE)
# Chargement des données pour prediction dans un data frame "declarations_a_predire"
declarations_a_predire <- read.csv("donnees_a_predire.csv", header = TRUE, sep = ",", dec = ".", stringsAsFactors = TRUE)

View(declarations_etude)
View(declarations_a_predire)

# Vérification du chargement des données dans le data frame declarations_etude en affichant la liste des variables et leur type
str(declarations_etude)
# Vérification du chargement des données dans le data frame declarations_a_predire en affichant la liste des variables et leur type
str(declarations_a_predire) # On remarque que le data frame contenant les déclarations à prédire ne contient pas de colonnes fraudilent

# 2. Pré-traitement des données ---------

# À présent que les données sont correctement chargés, il faut les pré-traiter
# On retire la colonne d'identifiation de la déclaration car elle n'est pas utile
declarations_etude <- declarations_etude[,-1]
str(declarations_etude)
declarations_a_predire <- declarations_a_predire[,-1]
str(declarations_a_predire)

# On visualise le data frame pour essayer de détecter des anomalies visuellement
View(declarations_etude)

# On ne constate aucune anomalie aberrantes dans les données

# On vérifie le nombre de données différentes présent dans chaque colonne
# L'intérêt est de s'assurer que le data frame ne contient pas de valeur comme identique, mais écrit différemment comme "nom" et "NON" ou "5" et "cinq"
# Colonne qu'on ne veut pas vérifier
colonnes_a_exclure <- c("customer_id", "age", "claim_amount", "days_to_incident")

for (col in names(declarations_etude)) {
  if (!(col %in% colonnes_a_exclure)) {
    cat("Fréquences des valeurs pour la colonne :", col, "\n")
    print(table(declarations_etude[[col]]))
    cat("\n")
  }
}
# On ne constate aucune anomalie dans les données

# On s'assure qu'aucunes valeurs dans la colonne age ne contienne de valeur iréel comme 0 ou 999
if (any(declarations_etude$age >= 99 | declarations_etude$age == 0)) {
  cat("Le data frame contient une lignes invalide\n")
} else {
  cat("RAS\n")
}

# On ne constate aucune anomalie dans les données

# 3. Définition de la méthode d'évaluation des classifieurs  ---------

# Nous testerons plusieurs algorithme de classifications durant notre analys : rpart, C50 et tree
library(rpart)
library(C50)
library(tree)

# Afin d'identifier le meilleur classificateur, c'est-à-dire le plus pertinent pour la détection de fraudes, nous évalurons leurs taux de succès/échecs et leurs matrices de confusions

# 4. Définition des données d'apprentissage et de test  ---------

# Afin de créer nos arbres de décisions, nous utiliserons les données du data frame declarations_etude, puisque ce dernier contient la colonne fraudulent
# Nous reparations les données de la manière suivante : 80% apprentissage, 20% test
nrow(declarations_etude) # Les données d'études contiennes 1100 lignes
nrow(declarations_etude) * 0.2 # Les données de tests seront constitués des 220 dernière ligne et donc les données d'apprentissage seront constitués des 880 premières lignes

declaration_EA <- declarations_etude[1:880,]
declaration_ET <- declarations_etude[881:1100,]

# 5. Construction et évaluation des classifieurs  ---------
# 5.A. RPART

# ...