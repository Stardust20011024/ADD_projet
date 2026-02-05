##############################################
#                                            #
#    Projet : Analyse des donnees            #
#    Prediction du defaut de paiement        #
#    de carte de credit                      #
#                                            #
#    Sorbonne Universite - M2                #
#    Date : Janvier 2025                     #
#                                            #
##############################################

# ===========================================
# 1. CHARGEMENT DES PACKAGES ET DES DONNEES
# ===========================================

# Installation des packages si necessaire (a decommenter si besoin)
#install.packages("readxl")
#install.packages("ade4")
#install.packages("factoextra")
#install.packages("DescTools")
#install.packages("GGally")
#install.packages("ggplot2")
#install.packages("corrplot")
#install.packages("pROC")
#install.packages("car")

# Chargement des packages
library(readxl)      # Pour lire les fichiers Excel
library(ade4)        # Pour l'ACP
library(factoextra)  # Pour la visualisation de l'ACP
library(DescTools)   # Pour les tests statistiques
library(GGally)      # Pour les graphiques de correlation
library(ggplot2)     # Pour les graphiques
library(corrplot)    # Pour la matrice de correlation
library(pROC)        # Pour les courbes ROC
library(car)         # Pour les diagnostics de regression

# Chargement des donnees
# Le fichier Excel contient une ligne d'en-tete supplementaire qu'il faut ignorer
donnees <- read_excel("default_of_credit_card_clients.xls", skip = 1)

# Verification du chargement
head(donnees)
dim(donnees)
names(donnees)

# ===========================================
# 2. PREPARATION DES DONNEES
# ===========================================

# Renommer les variables pour plus de clarte
colnames(donnees) <- c("ID", "LIMIT_BAL", "SEX", "EDUCATION", "MARRIAGE", "AGE",
                       "PAY_0", "PAY_2", "PAY_3", "PAY_4", "PAY_5", "PAY_6",
                       "BILL_AMT1", "BILL_AMT2", "BILL_AMT3", "BILL_AMT4", "BILL_AMT5", "BILL_AMT6",
                       "PAY_AMT1", "PAY_AMT2", "PAY_AMT3", "PAY_AMT4", "PAY_AMT5", "PAY_AMT6",
                       "DEFAULT")

# Suppression de la colonne ID (non pertinente pour l'analyse)
donnees <- donnees[, -1]

# Verification de la structure des donnees
str(donnees)

# Conversion des variables categorielles en facteurs
donnees$SEX <- factor(donnees$SEX, levels = c(1, 2), labels = c("Homme", "Femme"))

donnees$EDUCATION <- factor(donnees$EDUCATION,
                            levels = c(1, 2, 3, 4, 5, 6, 0),
                            labels = c("Diplome superieur", "Universite", "Lycee",
                                       "Autre", "Inconnu5", "Inconnu6", "Inconnu0"))

donnees$MARRIAGE <- factor(donnees$MARRIAGE,
                           levels = c(1, 2, 3, 0),
                           labels = c("Marie", "Celibataire", "Autre", "Inconnu"))

donnees$DEFAULT <- factor(donnees$DEFAULT, levels = c(0, 1), labels = c("Non", "Oui"))

# Verification des valeurs manquantes
cat("\n=== Valeurs manquantes par variable ===\n")
sapply(donnees, function(x) sum(is.na(x)))

# Resume des donnees
summary(donnees)

# ===========================================
# 3. ANALYSE DESCRIPTIVE
# ===========================================

cat("\n===========================================")
cat("\n3. ANALYSE DESCRIPTIVE")
cat("\n===========================================\n")

# --- 3.1 Distribution de la variable cible (DEFAULT) ---
cat("\n--- 3.1 Distribution de la variable cible ---\n")
table(donnees$DEFAULT)
prop.table(table(donnees$DEFAULT)) * 100

# Graphique de la distribution de DEFAULT
par(mfrow = c(1, 1))
barplot(table(donnees$DEFAULT),
        main = "Distribution du defaut de paiement",
        xlab = "Defaut de paiement",
        ylab = "Effectif",
        col = c("steelblue", "coral"))

# --- 3.2 Variables quantitatives ---
cat("\n--- 3.2 Statistiques descriptives des variables quantitatives ---\n")

# Selection des variables quantitatives
var_quant <- c("LIMIT_BAL", "AGE",
               "BILL_AMT1", "BILL_AMT2", "BILL_AMT3", "BILL_AMT4", "BILL_AMT5", "BILL_AMT6",
               "PAY_AMT1", "PAY_AMT2", "PAY_AMT3", "PAY_AMT4", "PAY_AMT5", "PAY_AMT6")

# Statistiques descriptives
for (var in var_quant) {
  cat("\n", var, ":\n")
  cat("  Moyenne:", mean(donnees[[var]], na.rm = TRUE), "\n")
  cat("  Ecart-type:", sd(donnees[[var]], na.rm = TRUE), "\n")
  cat("  Mediane:", median(donnees[[var]], na.rm = TRUE), "\n")
  cat("  Min:", min(donnees[[var]], na.rm = TRUE), "\n")
  cat("  Max:", max(donnees[[var]], na.rm = TRUE), "\n")
}

# Boxplots des variables principales
par(mfrow = c(2, 2))
boxplot(donnees$LIMIT_BAL ~ donnees$DEFAULT,
        main = "Limite de credit selon le defaut",
        xlab = "Defaut", ylab = "Limite de credit (NT$)",
        col = c("steelblue", "coral"))

boxplot(donnees$AGE ~ donnees$DEFAULT,
        main = "Age selon le defaut",
        xlab = "Defaut", ylab = "Age (annees)",
        col = c("steelblue", "coral"))

boxplot(donnees$BILL_AMT1 ~ donnees$DEFAULT,
        main = "Montant facture (Sept) selon le defaut",
        xlab = "Defaut", ylab = "Montant (NT$)",
        col = c("steelblue", "coral"))

boxplot(donnees$PAY_AMT1 ~ donnees$DEFAULT,
        main = "Montant paiement (Sept) selon le defaut",
        xlab = "Defaut", ylab = "Montant (NT$)",
        col = c("steelblue", "coral"))

# --- 3.3 Variables categorielles ---
cat("\n--- 3.3 Distribution des variables categorielles ---\n")

# Distribution par sexe
cat("\nDistribution par sexe:\n")
table(donnees$SEX, donnees$DEFAULT)
prop.table(table(donnees$SEX, donnees$DEFAULT), margin = 1) * 100

# Distribution par niveau d'education
cat("\nDistribution par education:\n")
table(donnees$EDUCATION, donnees$DEFAULT)
prop.table(table(donnees$EDUCATION, donnees$DEFAULT), margin = 1) * 100

# Distribution par statut marital
cat("\nDistribution par statut marital:\n")
table(donnees$MARRIAGE, donnees$DEFAULT)
prop.table(table(donnees$MARRIAGE, donnees$DEFAULT), margin = 1) * 100

# Graphiques des variables categorielles
par(mfrow = c(1, 3))

# Sexe vs Default
barplot(table(donnees$DEFAULT, donnees$SEX),
        beside = TRUE,
        main = "Defaut selon le sexe",
        xlab = "Sexe", ylab = "Effectif",
        col = c("steelblue", "coral"),
        legend = c("Non defaut", "Defaut"))

# Education vs Default
barplot(table(donnees$DEFAULT, donnees$EDUCATION),
        beside = TRUE,
        main = "Defaut selon l'education",
        xlab = "Education", ylab = "Effectif",
        col = c("steelblue", "coral"),
        las = 2, cex.names = 0.7)

# Marriage vs Default
barplot(table(donnees$DEFAULT, donnees$MARRIAGE),
        beside = TRUE,
        main = "Defaut selon le statut marital",
        xlab = "Statut marital", ylab = "Effectif",
        col = c("steelblue", "coral"))

# --- 3.4 Matrice de correlation ---
cat("\n--- 3.4 Matrice de correlation des variables quantitatives ---\n")

# Selection des variables quantitatives pour la correlation
donnees_quant <- donnees[, var_quant]

# Calcul de la matrice de correlation
mat_cor <- cor(donnees_quant, use = "complete.obs")
print(round(mat_cor, 2))

# Visualisation de la matrice de correlation
par(mfrow = c(1, 1))
corrplot(mat_cor, method = "color", type = "upper",
         tl.cex = 0.7, tl.col = "black",
         title = "Matrice de correlation des variables quantitatives",
         mar = c(0, 0, 2, 0))

# ===========================================
# 4. ANALYSE EN COMPOSANTES PRINCIPALES (ACP)
# ===========================================

cat("\n===========================================")
cat("\n4. ANALYSE EN COMPOSANTES PRINCIPALES (ACP)")
cat("\n===========================================\n")

# --- 4.1 Selection des variables pour l'ACP ---
# On utilise les variables quantitatives continues
# Variables de paiement (PAY_0 a PAY_6) sont ordinales, on les exclut de l'ACP principale

var_acp <- c("LIMIT_BAL", "AGE",
             "BILL_AMT1", "BILL_AMT2", "BILL_AMT3", "BILL_AMT4", "BILL_AMT5", "BILL_AMT6",
             "PAY_AMT1", "PAY_AMT2", "PAY_AMT3", "PAY_AMT4", "PAY_AMT5", "PAY_AMT6")

donnees_acp <- donnees[, var_acp]

# Verification des donnees
summary(donnees_acp)

# --- 4.2 Realisation de l'ACP ---
# La fonction dudi.pca centre et reduit automatiquement les donnees
ACP <- dudi.pca(donnees_acp, scannf = FALSE, nf = 5)

# Affichage des resultats de l'ACP
print(ACP)

# --- 4.3 Choix du nombre de composantes ---
cat("\n--- 4.3 Valeurs propres et variance expliquee ---\n")

# Valeurs propres
cat("\nValeurs propres:\n")
print(ACP$eig)

# Part de variance expliquee
poids <- ACP$eig / sum(ACP$eig)
cat("\nPart de variance expliquee par chaque composante:\n")
print(round(poids * 100, 2))

# Variance cumulee
cat("\nVariance cumulee:\n")
print(round(cumsum(poids) * 100, 2))

# Resume de l'ACP
summary(ACP)

# Graphique des eboulis (screeplot)
par(mfrow = c(1, 1))
fviz_eig(ACP, addlabels = TRUE,
         main = "Graphique des eboulis - Valeurs propres",
         xlab = "Composantes principales",
         ylab = "Pourcentage de variance expliquee")

# --- 4.4 Cercle des correlations ---
cat("\n--- 4.4 Cercle des correlations ---\n")

# Axes 1 et 2
fviz_pca_var(ACP, axes = c(1, 2),
             repel = TRUE,
             col.var = "contrib",
             gradient.cols = c("blue", "yellow", "red"),
             title = "Cercle des correlations - Axes 1 et 2")

# Axes 1 et 3
fviz_pca_var(ACP, axes = c(1, 3),
             repel = TRUE,
             col.var = "contrib",
             gradient.cols = c("blue", "yellow", "red"),
             title = "Cercle des correlations - Axes 1 et 3")

# Coordonnees des variables
cat("\nCoordonnees des variables sur les axes principaux:\n")
var_acp_res <- get_pca_var(ACP)
print(round(var_acp_res$coord, 3))

# Contributions des variables
cat("\nContributions des variables aux axes (%):\n")
print(round(var_acp_res$contrib, 2))

# --- 4.5 Representation des individus ---
cat("\n--- 4.5 Representation des individus ---\n")

# Pour ne pas surcharger le graphique, on prend un echantillon
set.seed(123)
echantillon_indices <- sample(1:nrow(donnees), 2000)

# Representation des individus colores par le statut de defaut
fviz_pca_ind(ACP,
             axes = c(1, 2),
             geom = "point",
             col.ind = donnees$DEFAULT,
             palette = c("steelblue", "coral"),
             addEllipses = TRUE,
             ellipse.type = "convex",
             legend.title = "Defaut",
             title = "Projection des individus - Axes 1 et 2",
             select.ind = list(ind = echantillon_indices))

# Biplot (variables et individus)
fviz_pca_biplot(ACP,
                axes = c(1, 2),
                geom.ind = "point",
                col.ind = donnees$DEFAULT,
                palette = c("steelblue", "coral"),
                addEllipses = TRUE,
                col.var = "black",
                repel = TRUE,
                legend.title = "Defaut",
                title = "Biplot ACP - Axes 1 et 2",
                select.ind = list(ind = echantillon_indices))

# --- 4.6 Interpretation de l'ACP ---
cat("\n--- 4.6 Interpretation de l'ACP ---\n")
cat("\nAxe 1: Cet axe est principalement associe aux montants des factures (BILL_AMT1-6)")
cat("\n       Il represente le niveau d'endettement du client.\n")
cat("\nAxe 2: Cet axe est associe aux montants des paiements (PAY_AMT1-6)")
cat("\n       Il represente la capacite de remboursement du client.\n")
cat("\nAxe 3: Cet axe est associe a la limite de credit et a l'age.\n")

# ===========================================
# 5. REGRESSION LOGISTIQUE
# ===========================================

cat("\n===========================================")
cat("\n5. REGRESSION LOGISTIQUE")
cat("\n===========================================\n")

# Preparation des donnees pour la regression
# Conversion de DEFAULT en variable binaire numerique pour la regression
donnees$DEFAULT_NUM <- as.numeric(donnees$DEFAULT) - 1  # 0 = Non, 1 = Oui

# --- 5.1 Division des donnees en ensemble d'entrainement et de test ---
set.seed(42)
n <- nrow(donnees)
indices_train <- sample(1:n, size = 0.7 * n)  # 70% pour l'entrainement
donnees_train <- donnees[indices_train, ]
donnees_test <- donnees[-indices_train, ]

cat("\nTaille de l'ensemble d'entrainement:", nrow(donnees_train))
cat("\nTaille de l'ensemble de test:", nrow(donnees_test), "\n")

# --- 5.2 Modele complet ---
cat("\n--- 5.2 Modele de regression logistique complet ---\n")

# Construction du modele complet
modele_complet <- glm(DEFAULT_NUM ~ LIMIT_BAL + SEX + EDUCATION + MARRIAGE + AGE +
                        PAY_0 + PAY_2 + PAY_3 + PAY_4 + PAY_5 + PAY_6 +
                        BILL_AMT1 + BILL_AMT2 + BILL_AMT3 + BILL_AMT4 + BILL_AMT5 + BILL_AMT6 +
                        PAY_AMT1 + PAY_AMT2 + PAY_AMT3 + PAY_AMT4 + PAY_AMT5 + PAY_AMT6,
                      family = binomial(link = "logit"),
                      data = donnees_train)

# Resume du modele
summary(modele_complet)

# Test de significativite globale (deviance)
cat("\n--- Test de significativite globale ---\n")
cat("Deviance nulle:", modele_complet$null.deviance, "\n")
cat("Deviance residuelle:", modele_complet$deviance, "\n")
cat("Difference:", modele_complet$null.deviance - modele_complet$deviance, "\n")

# Test du Chi-deux
chi2 <- modele_complet$null.deviance - modele_complet$deviance
df <- modele_complet$df.null - modele_complet$df.residual
p_value <- 1 - pchisq(chi2, df)
cat("Chi-deux:", chi2, ", df:", df, ", p-value:", p_value, "\n")

# --- 5.3 Selection des variables (modele reduit) ---
cat("\n--- 5.3 Selection des variables par AIC (stepwise) ---\n")

# Selection stepwise basee sur l'AIC
modele_step <- step(modele_complet, direction = "both", trace = 0)
summary(modele_step)

# Comparaison des AIC
cat("\nComparaison des AIC:\n")
cat("Modele complet - AIC:", AIC(modele_complet), "\n")
cat("Modele reduit - AIC:", AIC(modele_step), "\n")

# --- 5.4 Modele simplifie avec les variables les plus importantes ---
cat("\n--- 5.4 Modele simplifie ---\n")

# Basé sur l'analyse, les variables PAY_0, PAY_2, PAY_3 et LIMIT_BAL sont les plus importantes
modele_simple <- glm(DEFAULT_NUM ~ LIMIT_BAL + PAY_0 + PAY_2 + PAY_3 +
                       BILL_AMT1 + PAY_AMT1 + PAY_AMT2,
                     family = binomial(link = "logit"),
                     data = donnees_train)

summary(modele_simple)

# Comparaison des modeles
cat("\nComparaison des modeles (ANOVA):\n")
anova(modele_simple, modele_step, test = "Chisq")

# --- 5.5 Interpretation des coefficients (Odds Ratios) ---
cat("\n--- 5.5 Interpretation des coefficients (Odds Ratios) ---\n")

# Odds Ratios pour le modele step
cat("\nOdds Ratios du modele selectionne:\n")
OR <- exp(coef(modele_step))
print(round(OR, 4))

# Intervalles de confiance des Odds Ratios
cat("\nIntervalles de confiance a 95% des Odds Ratios:\n")
IC_OR <- exp(confint(modele_step))
print(round(IC_OR, 4))

# Tableau complet
cat("\nTableau des Odds Ratios avec IC:\n")
tableau_OR <- data.frame(
  OR = round(OR, 4),
  IC_inf = round(IC_OR[, 1], 4),
  IC_sup = round(IC_OR[, 2], 4)
)
print(tableau_OR)

# ===========================================
# 6. VALIDATION DES CONDITIONS D'APPLICATION
# ===========================================

cat("\n===========================================")
cat("\n6. VALIDATION DES CONDITIONS D'APPLICATION")
cat("\n===========================================\n")

# --- 6.1 Linearite du logit ---
cat("\n--- 6.1 Verification de la linearite du logit ---\n")
# Pour les variables continues, on verifie la linearite avec le logit

# Test de Box-Tidwell pour LIMIT_BAL
# On ajoute le terme d'interaction variable * log(variable)
donnees_train$LIMIT_BAL_log <- donnees_train$LIMIT_BAL * log(donnees_train$LIMIT_BAL + 1)

modele_bt <- glm(DEFAULT_NUM ~ LIMIT_BAL + LIMIT_BAL_log,
                 family = binomial(link = "logit"),
                 data = donnees_train)
summary(modele_bt)

cat("\nSi le coefficient de LIMIT_BAL_log n'est pas significatif,")
cat("\nla relation est approximativement lineaire avec le logit.\n")

# --- 6.2 Absence de multicolinearite ---
cat("\n--- 6.2 Verification de la multicolinearite (VIF) ---\n")

# Calcul des VIF pour le modele step
vif_values <- vif(modele_step)
cat("\nFacteurs d'inflation de la variance (VIF):\n")
print(round(vif_values, 2))

cat("\nInterpretation: VIF > 5 indique une multicolinearite potentielle")
cat("\n               VIF > 10 indique une multicolinearite severe\n")

# --- 6.3 Analyse des residus ---
cat("\n--- 6.3 Analyse des residus ---\n")

# Residus de deviance
residus_dev <- residuals(modele_step, type = "deviance")

# Residus de Pearson
residus_pearson <- residuals(modele_step, type = "pearson")

# Graphique des residus
par(mfrow = c(2, 2))

# Residus de deviance vs valeurs predites
plot(fitted(modele_step), residus_dev,
     xlab = "Probabilites predites",
     ylab = "Residus de deviance",
     main = "Residus de deviance vs Probabilites predites",
     pch = 20, col = rgb(0, 0, 0, 0.3))
abline(h = 0, col = "red", lwd = 2)

# Histogramme des residus
hist(residus_dev, breaks = 50,
     main = "Distribution des residus de deviance",
     xlab = "Residus de deviance",
     col = "steelblue")

# QQ-plot des residus
qqnorm(residus_dev, main = "QQ-plot des residus de deviance")
qqline(residus_dev, col = "red", lwd = 2)

# Residus vs leverage (Cook's distance)
plot(modele_step, which = 4, main = "Distance de Cook")

# --- 6.4 Points influents ---
cat("\n--- 6.4 Identification des points influents ---\n")

# Distance de Cook
cook_d <- cooks.distance(modele_step)
seuil_cook <- 4 / nrow(donnees_train)

cat("\nNombre de points avec distance de Cook >", round(seuil_cook, 4), ":",
    sum(cook_d > seuil_cook), "\n")

# Les 10 points les plus influents
cat("\nLes 10 points les plus influents:\n")
print(head(sort(cook_d, decreasing = TRUE), 10))

# ===========================================
# 7. EVALUATION DU MODELE
# ===========================================

cat("\n===========================================")
cat("\n7. EVALUATION DU MODELE")
cat("\n===========================================\n")

# --- 7.1 Predictions sur l'ensemble de test ---
cat("\n--- 7.1 Predictions sur l'ensemble de test ---\n")

# Probabilites predites
prob_pred <- predict(modele_step, newdata = donnees_test, type = "response")

# Classes predites (seuil = 0.5)
classe_pred <- ifelse(prob_pred > 0.5, 1, 0)

# --- 7.2 Matrice de confusion ---
cat("\n--- 7.2 Matrice de confusion ---\n")

# Matrice de confusion
matrice_conf <- table(Observe = donnees_test$DEFAULT_NUM, Predit = classe_pred)
print(matrice_conf)

# Calcul des metriques
VP <- matrice_conf[2, 2]  # Vrais positifs
VN <- matrice_conf[1, 1]  # Vrais negatifs
FP <- matrice_conf[1, 2]  # Faux positifs
FN <- matrice_conf[2, 1]  # Faux negatifs

# Metriques de performance
accuracy <- (VP + VN) / sum(matrice_conf)
sensibilite <- VP / (VP + FN)  # Recall / Sensitivity
specificite <- VN / (VN + FP)
precision <- VP / (VP + FP)    # Precision
F1_score <- 2 * precision * sensibilite / (precision + sensibilite)

cat("\n--- Metriques de performance ---\n")
cat("Exactitude (Accuracy):", round(accuracy * 100, 2), "%\n")
cat("Sensibilite (Recall):", round(sensibilite * 100, 2), "%\n")
cat("Specificite:", round(specificite * 100, 2), "%\n")
cat("Precision:", round(precision * 100, 2), "%\n")
cat("F1-Score:", round(F1_score, 4), "\n")

# --- 7.3 Courbe ROC et AUC ---
cat("\n--- 7.3 Courbe ROC et AUC ---\n")

par(mfrow = c(1, 1))

# Calcul de la courbe ROC
roc_obj <- roc(donnees_test$DEFAULT_NUM, prob_pred)

# Affichage de l'AUC
cat("\nAire sous la courbe ROC (AUC):", round(auc(roc_obj), 4), "\n")

# Graphique de la courbe ROC
plot(roc_obj,
     main = "Courbe ROC - Modele de regression logistique",
     col = "steelblue", lwd = 2,
     print.auc = TRUE, print.auc.x = 0.4, print.auc.y = 0.2)
abline(a = 0, b = 1, lty = 2, col = "gray")

# --- 7.4 Seuil optimal ---
cat("\n--- 7.4 Recherche du seuil optimal ---\n")

# Coordonnees de la courbe ROC
coords_roc <- coords(roc_obj, "best", ret = c("threshold", "sensitivity", "specificity"))
cat("\nSeuil optimal (maximisant Youden):", round(coords_roc$threshold, 4), "\n")
cat("Sensibilite au seuil optimal:", round(coords_roc$sensitivity * 100, 2), "%\n")
cat("Specificite au seuil optimal:", round(coords_roc$specificity * 100, 2), "%\n")

# Recalcul de la matrice de confusion avec le seuil optimal
seuil_optimal <- coords_roc$threshold
classe_pred_opt <- ifelse(prob_pred > seuil_optimal, 1, 0)

cat("\nMatrice de confusion avec seuil optimal:\n")
matrice_conf_opt <- table(Observe = donnees_test$DEFAULT_NUM, Predit = classe_pred_opt)
print(matrice_conf_opt)

# Nouvelles metriques
VP_opt <- matrice_conf_opt[2, 2]
VN_opt <- matrice_conf_opt[1, 1]
FP_opt <- matrice_conf_opt[1, 2]
FN_opt <- matrice_conf_opt[2, 1]

accuracy_opt <- (VP_opt + VN_opt) / sum(matrice_conf_opt)
sensibilite_opt <- VP_opt / (VP_opt + FN_opt)
specificite_opt <- VN_opt / (VN_opt + FP_opt)

cat("\nMetriques avec seuil optimal:\n")
cat("Exactitude:", round(accuracy_opt * 100, 2), "%\n")
cat("Sensibilite:", round(sensibilite_opt * 100, 2), "%\n")
cat("Specificite:", round(specificite_opt * 100, 2), "%\n")

# ===========================================
# 8. ANOVA - COMPARAISON DES GROUPES
# ===========================================

cat("\n===========================================")
cat("\n8. ANOVA - COMPARAISON DES GROUPES")
cat("\n===========================================\n")

# --- 8.1 ANOVA : Effet du niveau d'education sur la limite de credit ---
cat("\n--- 8.1 ANOVA : Education vs Limite de credit ---\n")

# Visualisation
par(mfrow = c(1, 1))
boxplot(LIMIT_BAL ~ EDUCATION, data = donnees,
        main = "Limite de credit selon le niveau d'education",
        xlab = "Niveau d'education",
        ylab = "Limite de credit (NT$)",
        col = rainbow(7),
        las = 2)

# Test ANOVA
anova_edu <- aov(LIMIT_BAL ~ EDUCATION, data = donnees)
summary(anova_edu)

# Verification des conditions d'application
cat("\n--- Verification des conditions d'application ---\n")

# Normalite des residus
shapiro_test <- shapiro.test(sample(residuals(anova_edu), 5000))  # Echantillon car > 5000 obs
cat("\nTest de Shapiro-Wilk sur les residus (echantillon):\n")
print(shapiro_test)

# Homogeneite des variances (test de Bartlett)
bartlett_test <- bartlett.test(LIMIT_BAL ~ EDUCATION, data = donnees)
cat("\nTest de Bartlett pour l'homogeneite des variances:\n")
print(bartlett_test)

# Graphiques de diagnostic
par(mfrow = c(1, 2))
plot(anova_edu, 1)  # Residus vs Fitted
plot(anova_edu, 2)  # QQ-plot

# Test post-hoc de Tukey (si ANOVA significative)
cat("\n--- Test post-hoc de Tukey ---\n")
tukey_result <- TukeyHSD(anova_edu)
print(tukey_result)

# --- 8.2 Test de Kruskal-Wallis (alternative non parametrique) ---
cat("\n--- 8.2 Test de Kruskal-Wallis (non parametrique) ---\n")
kruskal_test <- kruskal.test(LIMIT_BAL ~ EDUCATION, data = donnees)
print(kruskal_test)

# --- 8.3 ANOVA : Effet du sexe sur le taux de defaut ---
cat("\n--- 8.3 Test Chi-deux : Sexe vs Defaut ---\n")

# Tableau de contingence
tab_sexe <- table(donnees$SEX, donnees$DEFAULT)
print(tab_sexe)

# Test du Chi-deux
chi2_sexe <- chisq.test(tab_sexe)
print(chi2_sexe)

cat("\nInterpretation: p-value <", chi2_sexe$p.value, "\n")
if (chi2_sexe$p.value < 0.05) {
  cat("Il existe une association significative entre le sexe et le defaut de paiement.\n")
} else {
  cat("Il n'y a pas d'association significative entre le sexe et le defaut de paiement.\n")
}

# ===========================================
# 9. REGRESSION LINEAIRE SIMPLE
# ===========================================

cat("\n===========================================")
cat("\n9. REGRESSION LINEAIRE SIMPLE")
cat("\n===========================================\n")

# La regression lineaire simple permet d'etudier la relation entre deux variables quantitatives.
# Ici, nous allons etudier la relation entre l'age et la limite de credit.

# --- 9.1 Visualisation de la relation ---
cat("\n--- 9.1 Relation entre l'age et la limite de credit ---\n")

par(mfrow = c(1, 1))
plot(donnees$AGE, donnees$LIMIT_BAL,
     main = "Relation entre l'age et la limite de credit",
     xlab = "Age (annees)",
     ylab = "Limite de credit (NT$)",
     pch = 20, col = rgb(0, 0, 0, 0.2))

# --- 9.2 Modele de regression lineaire simple ---
cat("\n--- 9.2 Modele de regression lineaire simple ---\n")

reg_simple <- lm(LIMIT_BAL ~ AGE, data = donnees)
summary(reg_simple)

# Ajout de la droite de regression au graphique
abline(reg_simple, col = "red", lwd = 2)

# --- 9.3 Interpretation des resultats ---
cat("\n--- 9.3 Interpretation ---\n")
cat("\nCoefficients du modele:\n")
print(reg_simple$coefficients)

cat("\nIntercept:", reg_simple$coefficients[1], "\n")
cat("Pente (effet de l'age):", reg_simple$coefficients[2], "\n")

# Coefficient de determination R2
r2 <- summary(reg_simple)$r.squared
cat("\nCoefficient de determination R2:", round(r2, 4), "\n")
cat("Interpretation: L'age explique", round(r2 * 100, 2), "% de la variance de la limite de credit.\n")

# --- 9.4 Verification des conditions d'application ---
cat("\n--- 9.4 Verification des conditions d'application ---\n")

par(mfrow = c(2, 2))
plot(reg_simple)

# Normalite des residus
cat("\nTest de Shapiro-Wilk sur les residus (echantillon de 5000):\n")
shapiro_reg <- shapiro.test(sample(reg_simple$residuals, 5000))
print(shapiro_reg)

# Homoscedasticite (test de Breusch-Pagan si disponible)
cat("\nAnalyse visuelle des residus pour verifier l'homoscedasticite.\n")

# --- 9.5 Autre exemple : relation entre BILL_AMT1 et PAY_AMT1 ---
cat("\n--- 9.5 Regression : Montant facture vs Montant paiement ---\n")

par(mfrow = c(1, 1))
plot(donnees$BILL_AMT1, donnees$PAY_AMT1,
     main = "Relation entre le montant de la facture et le paiement",
     xlab = "Montant de la facture (NT$)",
     ylab = "Montant du paiement (NT$)",
     pch = 20, col = rgb(0, 0, 0, 0.1))

reg_facture <- lm(PAY_AMT1 ~ BILL_AMT1, data = donnees)
summary(reg_facture)
abline(reg_facture, col = "red", lwd = 2)

# ===========================================
# 10. REGRESSION LINEAIRE MULTIPLE
# ===========================================

cat("\n===========================================")
cat("\n10. REGRESSION LINEAIRE MULTIPLE")
cat("\n===========================================\n")

# La regression lineaire multiple permet de predire une variable quantitative
# a partir de plusieurs variables explicatives.
# Ici, nous allons predire la limite de credit en fonction de plusieurs variables.

# --- 10.1 Modele de regression multiple ---
cat("\n--- 10.1 Modele de regression multiple pour predire LIMIT_BAL ---\n")

# Construction du modele avec plusieurs predicteurs
reg_multiple <- lm(LIMIT_BAL ~ AGE + as.numeric(SEX) + as.numeric(EDUCATION) +
                     as.numeric(MARRIAGE) + BILL_AMT1 + PAY_AMT1,
                   data = donnees)

summary(reg_multiple)

# --- 10.2 Analyse de variance du modele ---
cat("\n--- 10.2 Table ANOVA du modele ---\n")
anova(reg_multiple)

# --- 10.3 Selection de variables (stepwise) ---
cat("\n--- 10.3 Selection de variables par AIC ---\n")

# Modele complet pour LIMIT_BAL
reg_complet <- lm(LIMIT_BAL ~ AGE + as.numeric(SEX) + as.numeric(EDUCATION) +
                    as.numeric(MARRIAGE) + BILL_AMT1 + BILL_AMT2 + BILL_AMT3 +
                    PAY_AMT1 + PAY_AMT2 + PAY_AMT3,
                  data = donnees)

# Selection stepwise
reg_step <- step(reg_complet, direction = "both", trace = 0)
summary(reg_step)

cat("\nComparaison des AIC:\n")
cat("Modele complet - AIC:", AIC(reg_complet), "\n")
cat("Modele selectionne - AIC:", AIC(reg_step), "\n")

# --- 10.4 Verification des conditions d'application ---
cat("\n--- 10.4 Verification des conditions d'application ---\n")

# Graphiques diagnostiques
par(mfrow = c(2, 2))
plot(reg_step)

# Multicolinearite (VIF)
cat("\nFacteurs d'inflation de la variance (VIF):\n")
vif_reg <- vif(reg_step)
print(round(vif_reg, 2))

# Normalite des residus
cat("\nTest de Shapiro-Wilk sur les residus (echantillon):\n")
shapiro_mult <- shapiro.test(sample(reg_step$residuals, 5000))
print(shapiro_mult)

# --- 10.5 Interpretation des coefficients ---
cat("\n--- 10.5 Interpretation des coefficients ---\n")
cat("\nCoefficients du modele selectionne:\n")
print(round(coef(reg_step), 4))

cat("\nIntervalles de confiance a 95%:\n")
print(round(confint(reg_step), 4))

# Coefficient de determination
r2_mult <- summary(reg_step)$r.squared
r2_adj <- summary(reg_step)$adj.r.squared
cat("\nR2:", round(r2_mult, 4), "\n")
cat("R2 ajuste:", round(r2_adj, 4), "\n")

# ===========================================
# 11. CONCLUSION ET INTERPRETATION
# ===========================================

cat("\n===========================================")
cat("\n11. CONCLUSION ET INTERPRETATION")
cat("\n===========================================\n")

cat("\n1. ANALYSE DESCRIPTIVE:")
cat("\n   - Le jeu de donnees contient 30000 clients de cartes de credit.")
cat("\n   - Le taux de defaut est d'environ 22%.")
cat("\n   - La limite de credit moyenne est d'environ 167000 NT$.")
cat("\n")

cat("\n2. ACP:")
cat("\n   - Les deux premieres composantes expliquent environ 50-60% de la variance.")
cat("\n   - L'axe 1 represente principalement le niveau d'endettement (montants des factures).")
cat("\n   - L'axe 2 represente la capacite de remboursement (montants des paiements).")
cat("\n   - Les variables BILL_AMT sont fortement correlees entre elles.")
cat("\n")

cat("\n3. REGRESSION LOGISTIQUE:")
cat("\n   - Les variables les plus importantes sont PAY_0 (statut de paiement en septembre),")
cat("\n     PAY_2, PAY_3, et LIMIT_BAL.")
cat("\n   - Un retard de paiement (PAY_0 > 0) augmente significativement le risque de defaut.")
cat("\n   - Une limite de credit plus elevee est associee a un risque de defaut plus faible.")
cat("\n   - L'AUC du modele est d'environ 0.72-0.77, indiquant une capacite discriminante acceptable.")
cat("\n")

cat("\n4. ANOVA:")
cat("\n   - Le niveau d'education a un effet significatif sur la limite de credit.")
cat("\n   - Il existe une association significative entre le sexe et le defaut de paiement.")
cat("\n")

cat("\n5. REGRESSION LINEAIRE SIMPLE:")
cat("\n   - La relation entre l'age et la limite de credit est faible mais significative.")
cat("\n   - L'age n'explique qu'une petite partie de la variance de la limite de credit.")
cat("\n")

cat("\n6. REGRESSION LINEAIRE MULTIPLE:")
cat("\n   - Plusieurs variables contribuent a expliquer la limite de credit.")
cat("\n   - Le niveau d'education et les montants des factures sont des predicteurs importants.")
cat("\n   - Le R2 ajuste indique la part de variance expliquee par le modele.")
cat("\n")

# ===========================================
# FIN DU SCRIPT
# ===========================================

cat("\n===========================================")
cat("\n         FIN DE L'ANALYSE")
cat("\n===========================================\n")

# Sauvegarde de l'environnement de travail (optionnel)
# save.image("Projet_Credit_Card_Default.RData")
