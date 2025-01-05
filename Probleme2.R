# Prépatation de l'environnement et des données
setwd("F:/M1 MIAGE UEVE/Statistique/TP")

data2012 <- scan("company2012.dat",skip = 1)
data2017 <- scan("company2017.dat",skip = 1)
data2022 <- scan("company2022.dat",skip = 1)

#Q1 Affichez la distribution des performances pour les 3 années.
hist(data2012, 
     freq = FALSE,
     main = "Distribution des performances (2012)", 
     xlab = "Performance", 
     col = "lightgray", 
     border = "white")
lines(density(data2012), col = "red", lwd = 2)

hist(data2017, 
     freq = FALSE,
     main = "Distribution des performances (2017)", 
     xlab = "Performance", 
     col = "lightgray", 
     border = "white")
lines(density(data2017), col = "red", lwd = 2)

hist(data2022, 
     freq = FALSE,
     main = "Distribution des performances (2022)", 
     xlab = "Performance", 
     col = "lightgray", 
     border = "white")
lines(density(data2022), col = "red", lwd = 2)

#Q2 Visuellement, les performances semblent suivre une loi de probabilité normale. Effectuez un test de Shapiro-Wilk afin de vérifier cette hypothèse
shapiro_test_2012 <- shapiro.test(data2012)
shapiro_test_2017 <- shapiro.test(data2017)
shapiro_test_2022 <- shapiro.test(data2022)

print("***** Résultat du test de shapiro-wilk pour les données de l'année 2012  ******")
print(shapiro_test_2012)

print("***** Résultat du test de shapiro-wilk pour les données de l'année 2017  ******")
print(shapiro_test_2017)

print("***** Résultat du test de shapiro-wilk pour les données de l'année 2022  ******")
print(shapiro_test_2022)

#Q3 Test ANOVA
data_indep <- data.frame(
  performance = c(data2012, data2017, data2022),
  periode = factor(rep(c("2012", "2017", "2022"), each = length(data2012)))
)

anova_indep <- aov(performance ~ periode, data = data_indep)
summary(anova_indep)


#Q4 Test Kruskal-Wallis
kruskal_test <- kruskal.test(
  x = c(data2012, data2017, data2022),
  g = factor(rep(c("2012", "2017", "2022"), each = length(data2012)))
)
print(kruskal_test)

