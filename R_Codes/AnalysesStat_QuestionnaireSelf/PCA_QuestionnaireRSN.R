library(xlsx)
library(FactoMineR)

library(Factoshiny)
library(missMDA)
library(FactoInvestigate)
#library(Rcmdr)

data <- read.table("E:/Data_friedman.csv", header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE)

#-------------------------- Select the data and compute the PCA ---------------------------------------
# Select columns for the PCA
colnames(data)
data_MFA <- subset(data, select=c("Present.dedans", "Propre.Voix", "Soi.meme", "Autres.Personnes.Familliere", "Autres.Personnes.Inconnues"))

# compute the PCA
res.pca = PCA(data_MFA, scale.unit=TRUE, ncp=5, graph=T) 

#-------------------------- Print the results ---------------------------------------
summary(res.pca)

fviz_contrib (res.mfa, choice = "quanti.var", axes = 1, top = 20, palette = "jco")