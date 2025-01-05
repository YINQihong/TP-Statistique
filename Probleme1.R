# Prépatation de l'environnement et des données
setwd("F:/M1 MIAGE UEVE/Statistique/TP")
data <- read.csv("psychology.csv")

# Q1 Calculez des statistiques descriptives des distributions de l’IBE et de l’IP : moyenne, médiane, écart-type, IQR.

means <- apply(data, 2, mean)
medians <- apply(data, 2, median)
sds <- apply(data, 2, sd)
iqrs <- apply(data, 2, IQR)

result <- data.frame(
  Mean = means,
  Median = medians,
  SD = sds,
  IQR = iqrs
)
print("************** Question 1 **************")
print(result)
print("****************************************")

# Q2 Affichez une estimation de densité par histogramme pour chacune des deux variables : IBE et IP 
hist_IBE = hist(data$IBE, freq = FALSE, main = "Histogramme et densité - IBE",xlab = "IBE", col = "lightblue", border = "black")

hist_IP = hist(data$IP, freq = FALSE, main = "Histogramme et densité - IP",
     xlab = "IP", col = "lightgreen", border = "black")

# Q3 Affichez les mêmes histogrammes que ceux de la question précédente, mais avec des largeurs d’intervalles divisées par deux
new_breaks_IBE <- seq(min(hist_IBE$breaks), max(hist_IBE$breaks), length.out = length(hist_IBE$breaks) * 2)
hist_IBE_breaks = hist(data$IBE, breaks =new_breaks_IBE,  freq = FALSE, main = "Histogramme (intervalles réduits) - IBE",xlab = "IBE", col = "lightblue", border = "black")

new_breaks_IP <- seq(min(hist_IP$breaks), max(hist_IP$breaks), length.out = length(hist_IP$breaks) * 2)
hist_IP_breaks = hist(data$IP,breaks = new_breaks_IP, freq = FALSE, main = "Histogramme (intervalles réduits) - IP",
                      xlab = "IP", col = "lightgreen", border = "black")

#Q4  Affichez une estimation par noyau de la densité (en anglais : kernel density estimation) pour chacune des deux variables : IBE et IP
kde_IBE <- density(data$IBE)
kde_IP <- density(data$IP)

plot(kde_IBE, main = "Estimation par noyau de la densité - IBE", 
     xlab = "IBE", ylab = "Densité", col = "blue", lwd = 2)

plot(kde_IP,main = "Estimation par noyau de la densité - IP",
     xlab = "IP", ylab = "Densité", col = "red", lwd = 2)

#Q5 Refaites la Q4 - en utilisant un noyau triangulaire et en changeant la méthode du choix de paramètre de lissage par un validation croisée biaisée
kde_IBE_BIS <- density(data$IBE, kernel = "triangular", bw = "SJ")
kde_IP_BIS <- density(data$IP, kernel = "triangular", bw = "SJ")


plot(kde_IBE_BIS, main = "Estimation par noyau triangulaire de la densité - IBE", 
     xlab = "IBE", ylab = "Densité", col = "blue", lwd = 2)

plot(kde_IP_BIS, main = "Estimation par noyau triangulaire de la densité - IP", 
     xlab = "IBE", ylab = "Densité", col = "red", lwd = 2)

#Q6 Afficher le nuage de points entre les deux variables IBE et IP. Au regard de ce graphique, comment décririez-vous la relation entre ces deux variables ?
plot(data$IBE, data$IP,
     main = "Nuage de points entre IBE et IP",
     xlab = "IBE", ylab = "IP",
     col = "blue", pch = 16)

#Q7 Trouvez une transformation des données permettant de linéariser la relation entre les deux variables et affichez le nuage de points 
plot(log(data$IBE), data$IP, 
     main = "Relation linéarisée entre log(IBE) et IP", 
     xlab = "log(IBE)", ylab = "IP", 
     col = "blue", pch = 16)

#Q8 À partir de ces données transformées, calculez le coefficient de corrélation de Pearson entre les deux variables. Calculez également les coefficients de corrélation de Spearman et Kendall.
coef_corr_Pearson <- cor(log(data$IBE), data$IP, method = "pearson")
coef_corr_Spearman <- cor(log(data$IBE), data$IP, method = "spearman")
coef_corr_Kendall <- cor(log(data$IBE), data$IP, method = "kendall")

print("************** Question 8 **************")
print("le cofficient de corrélation de Pearson")
print(coef_corr_Pearson)
print("le cofficient de corrélation de Spearman")
print(coef_corr_Spearman)
print("le cofficient de corrélation de Kendall.")
print(coef_corr_Kendall)
print("****************************************")

#Q9 En reprenant le graphique de la question précédente, ajoutez 
#   - une ligne diagonale (pente = 1), de couleur rouge,
#   - les noms des abscisse et ordonnée (c’est-à-dire, IBE et IP).
plot(log(data$IBE), data$IP, 
     main = "Relation linéarisée entre IBE et IP à partir des données transformées", 
     xlab = "IBE", ylab = "IP", 
     col = "blue", pch = 16)
abline(a = 0, b = 1, col = "red", lwd = 2)

