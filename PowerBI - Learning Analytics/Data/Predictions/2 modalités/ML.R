# Heure de début
Sys.time()

# Chargement des packages nécessaires
library(dplyr) # pour la manipulation de données
library(caret) # pour la construction de modèles
library(ranger) # pour la construction de modèles de forêts aléatoires

# Définition du chemin
chemin = "~/Desktop/Learning Analytics/"

############# Dataset B #############

# Chargement du dataset
learning_analytics_B <- read.csv(paste0(chemin,"learning_analytics_B.csv"), row.names=1)

# Recodage de la variable cible en 2 modalités : "Pass" et "Fail"
learning_analytics_B$final_result <- gsub("Withdrawn", "Fail", learning_analytics_B$final_result)
learning_analytics_B$final_result <- gsub("Distinction", "Pass", learning_analytics_B$final_result)

# Remplacement des "," par des "." dans "Moyenne de score" et conversion en numérique
learning_analytics_B$Moyenne.de.score <- gsub("\\,", ".", learning_analytics_B$Moyenne.de.score)

# Conversion en numérique
learning_analytics_B$Moyenne.de.score = as.numeric(learning_analytics_B$Moyenne.de.score)

# Remplacement des valeurs manquantes de la colonne "Moyenne pondérée de score" par les valeurs correspondantes de la colonne "Moyenne de score"
learning_analytics_B$Moyenne.pondérée.de.score <- ifelse(is.na(learning_analytics_B$Moyenne.pondérée.de.score), learning_analytics_B$Moyenne.de.score, learning_analytics_B$Moyenne.pondérée.de.score)

# Remplacement des valeurs manquantes de la colonne "imd_band" par "40-50%"
learning_analytics_B$imd_band <- ifelse(is.na(learning_analytics_B$imd_band), "40-50%", learning_analytics_B$imd_band)

# Remplacement des valeurs manquantes des colonnes "Nombre de date", "sum_click" et "Nombre de activity_type" par des 0
learning_analytics_B$Nombre.de.date <- ifelse(is.na(learning_analytics_B$Nombre.de.date), 0, learning_analytics_B$Nombre.de.date)
learning_analytics_B$sum_click <- ifelse(is.na(learning_analytics_B$sum_click), 0, learning_analytics_B$sum_click)
learning_analytics_B$Nombre.de.activity_type <- ifelse(is.na(learning_analytics_B$Nombre.de.activity_type), 0, learning_analytics_B$Nombre.de.activity_type)

# Suppression des lignes pour lesquelles il y a des valeurs manquantes dans les variables "Moyenne de score" et "date_registration"
learning_analytics_B <- subset(learning_analytics_B, !is.na(Moyenne.de.score))
learning_analytics_B <- subset(learning_analytics_B, !is.na(date_registration))

# Conversion de la variable cible en facteur ordinal
learning_analytics_B$final_result <- ordered(learning_analytics_B$final_result, 
                                            levels = c("Fail", "Pass")
)

# Conversion de variables explicatives en facteur ordinal
learning_analytics_B$imd_band <- ordered(learning_analytics_B$imd_band, 
                                             levels = c("0-10%", "10-20", "20-30%", "30-40%","40-50%","","50-60%","60-70%","70-80%","80-90%","90-100%")
)
learning_analytics_B$age_band <- ordered(learning_analytics_B$age_band, 
                                             levels = c("0-35", "35-55", "55<=")
)
learning_analytics_B$highest_education <- ordered(learning_analytics_B$highest_education, 
                                         levels = c("No Formal quals", "Lower Than A Level", "A Level or Equivalent","HE Qualification","Post Graduate Qualification")
)

# Afficher le type des variables
str(learning_analytics_B)

# Séparation des données en ensembles d'apprentissage et de test
set.seed(123) # pour la reproductibilité des résultats
training.samples <- learning_analytics_B$final_result %>% createDataPartition(p = 0.8, list = FALSE)
train.data <- learning_analytics_B[training.samples, ]
test.data <- learning_analytics_B[-training.samples, ]

# Création des modèles
model <- train(final_result ~ ., data=train.data, method="ranger") # Forêt aléatoire 1
model_rf <- train(final_result ~ ., data = train.data, method = "rf") # Forêt aléatoire 2
model_dt <- train(final_result ~ ., data = train.data, method = "rpart") # Arbre de décision
model_nnet <- train(final_result ~ ., data = train.data, method = "nnet", trace = FALSE) # Réseau de neurones

# Évaluation des performances du modèle sur les données de test
predictions <- predict(model, newdata=test.data)
print("Dataset B / Forêt aléatoire 1 (ranger)")
confusionMatrix(predictions, test.data$final_result)

predictions_rf <- predict(model_rf, newdata=test.data)
print("Dataset B / Forêt aléatoire 2 (rf)")
confusionMatrix(predictions_rf, test.data$final_result)

predictions_dt <- predict(model_dt, newdata=test.data)
print("Dataset B / Arbre de décision (rpart)")
confusionMatrix(predictions_dt, test.data$final_result)

predictions_nnet <- predict(model_nnet, newdata=test.data)
print("Dataset B / # Réseau de neurones (nnet)")
confusionMatrix(predictions_nnet, test.data$final_result)

# Créer un nouveau data.frame avec les index et les prédictions
result <- data.frame(index = row.names(test.data),
                     prediction = predictions)

result_rf <- data.frame(index = row.names(test.data),
                     prediction = predictions_rf)

result_dt <- data.frame(index = row.names(test.data),
                     prediction = predictions_dt)

result_nnet <- data.frame(index = row.names(test.data),
                     prediction = predictions_nnet)

# Exporter les résultats sous forme de fichier CSV
write.csv(result, file = paste0(chemin,"predictions_ML_B_ranger.csv"), row.names = FALSE)
write.csv(result_rf, file = paste0(chemin,"predictions_ML_B_rf.csv"), row.names = FALSE)
write.csv(result_dt, file = paste0(chemin,"predictions_ML_B_dt.csv"), row.names = FALSE)
write.csv(result_nnet, file = paste0(chemin,"predictions_ML_B_nnet.csv"), row.names = FALSE)

############# Dataset J #############

# Chargement du dataset
learning_analytics_J <- read.csv(paste0(chemin,"learning_analytics_J.csv"), row.names=1)

# Recodage de la variable cible en 2 modalités : "Pass" et "Fail"
learning_analytics_J$final_result <- gsub("Withdrawn", "Fail", learning_analytics_J$final_result)
learning_analytics_J$final_result <- gsub("Distinction", "Pass", learning_analytics_J$final_result)

# Remplacement des "," par des "." dans "Moyenne de score" et conversion en numérique
learning_analytics_J$Moyenne.de.score <- gsub("\\,", ".", learning_analytics_J$Moyenne.de.score)

# Conversion en numérique
learning_analytics_J$Moyenne.de.score = as.numeric(learning_analytics_J$Moyenne.de.score)

# Remplacement des valeurs manquantes de la colonne "Moyenne pondérée de score" par les valeurs correspondantes de la colonne "Moyenne de score"
learning_analytics_J$Moyenne.pondérée.de.score <- ifelse(is.na(learning_analytics_J$Moyenne.pondérée.de.score),learning_analytics_J$Moyenne.de.score,learning_analytics_J$Moyenne.pondérée.de.score)

# Remplacement des valeurs manquantes de la colonne "imd_band" par "40-50%"
learning_analytics_J$imd_band <- ifelse(is.na(learning_analytics_J$imd_band), "40-50%", learning_analytics_J$imd_band)

# Remplacement des valeurs manquantes des colonnes "Nombre de date", "sum_click" et "Nombre de activity_type" par des 0
learning_analytics_J$Nombre.de.date <- ifelse(is.na(learning_analytics_J$Nombre.de.date), 0, learning_analytics_J$Nombre.de.date)
learning_analytics_J$sum_click <- ifelse(is.na(learning_analytics_J$sum_click), 0, learning_analytics_J$sum_click)
learning_analytics_J$Nombre.de.activity_type <- ifelse(is.na(learning_analytics_J$Nombre.de.activity_type), 0, learning_analytics_J$Nombre.de.activity_type)

# Suppression des lignes pour lesquelles il y a des valeurs manquantes dans les variables "Moyenne de score" et "date_registration"
learning_analytics_J <- subset(learning_analytics_J, !is.na(Moyenne.de.score))
learning_analytics_J <- subset(learning_analytics_J, !is.na(date_registration))

# Conversion de la variable cible en facteur ordinal
learning_analytics_J$final_result <- ordered(learning_analytics_J$final_result, 
                                            levels = c("Fail", "Pass")
)

# Conversion de variables explicatives en facteur ordinal
learning_analytics_J$imd_band <- ordered(learning_analytics_J$imd_band, 
                                         levels = c("0-10%", "10-20", "20-30%", "30-40%","40-50%","","50-60%","60-70%","70-80%","80-90%","90-100%")
)
learning_analytics_J$age_band <- ordered(learning_analytics_J$age_band, 
                                         levels = c("0-35", "35-55", "55<=")
)
learning_analytics_J$highest_education <- ordered(learning_analytics_J$highest_education, 
                                                  levels = c("No Formal quals", "Lower Than A Level", "A Level or Equivalent","HE Qualification","Post Graduate Qualification")
)

# Afficher le type des variables
str(learning_analytics_J)

# Séparation des données en ensembles d'apprentissage et de test
set.seed(123) # Pour la reproductibilité des résultats
training.samples <- learning_analytics_J$final_result %>% createDataPartition(p = 0.8, list = FALSE)
train.data <- learning_analytics_J[training.samples, ]
test.data <- learning_analytics_J[-training.samples, ]

# Création des modèles 
model <- train(final_result ~ ., data=train.data, method="ranger") # Forêt aléatoire 1
model_rf <- train(final_result ~ ., data = train.data, method = "rf") # Forêt aléatoire 2
model_dt <- train(final_result ~ ., data = train.data, method = "rpart") # Arbre de décision
model_nnet <- train(final_result ~ ., data = train.data, method = "nnet", trace = FALSE) # Réseau de neurones

# Évaluation des performances des modèles sur les données de test
predictions <- predict(model, newdata=test.data)
print("Dataset J / Forêt aléatoire 1 (ranger)")
confusionMatrix(predictions, test.data$final_result)

predictions_rf <- predict(model_rf, newdata=test.data)
print("Dataset J / Forêt aléatoire 2 (rf)")
confusionMatrix(predictions_rf, test.data$final_result)

predictions_dt <- predict(model_dt, newdata=test.data)
print("Dataset J / Arbre de décision (rpart)")
confusionMatrix(predictions_dt, test.data$final_result)

predictions_nnet <- predict(model_nnet, newdata=test.data)
print("Dataset J / # Réseau de neurones (nnet)")
confusionMatrix(predictions_nnet, test.data$final_result)

# Créer un nouveau data.frame avec les index et les prédictions
result <- data.frame(index = row.names(test.data),
                     prediction = predictions)

result_rf <- data.frame(index = row.names(test.data),
                        prediction = predictions_rf)

result_dt <- data.frame(index = row.names(test.data),
                        prediction = predictions_dt)

result_nnet <- data.frame(index = row.names(test.data),
                          prediction = predictions_nnet)

# Exporter les résultats sous forme de fichier CSV
write.csv(result, file = paste0(chemin,"predictions_ML_J_ranger.csv"), row.names = FALSE)
write.csv(result_rf, file = paste0(chemin,"predictions_ML_J_rf.csv"), row.names = FALSE)
write.csv(result_dt, file = paste0(chemin,"predictions_ML_J_dt.csv"), row.names = FALSE)
write.csv(result_nnet, file = paste0(chemin,"predictions_ML_J_nnet.csv"), row.names = FALSE)

# Heure de fin
Sys.time()
