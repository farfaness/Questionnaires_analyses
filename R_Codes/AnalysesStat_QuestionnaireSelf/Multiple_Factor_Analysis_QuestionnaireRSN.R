#library(xlsx)
library(FactoMineR)

library(Factoshiny)
library(missMDA)
library(FactoInvestigate)
#library(Rcmdr)

#  call the DATA file
#data <- read.xlsx('F:/Data_comport_tableau_court_et_long.ods' , sheetIndex = 1, header=TRUE)
data <- read.table("F:/Data_friedman.csv", header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE)

#-------------------------- Select the data and compute the MFA  ---------------------------------------
# Select columns for the MFA
colnames(data)
data_MFA <- subset(data, select=c("High.Low", "Pensees.Visuelles", "Present.dedans", "Pensees.Parole", "Propre.Voix", "Pensees.Personnes", "Soi.meme", "Autres.Personnes.Familliere", "Autres.Personnes.Inconnues"))

# compute the MFA
res = MFA(data_MFA, group=c(1, 2, 2, 4), type=c("n",rep("s",3)), ncp=5, name.group=c("condition", "visual","auditory","person"), num.group.sup=c(), graph = TRUE, weight.col.mfa = NULL, row.w = NULL, axes = c(1,2))
#group: a vector indicating the number of variables in each group
#type: the type of the variables in each group. "s" for scaled continuous variables, "c" for centered (unscaled) continuous variables and "n" for categorical variables
#ncp: number of dimensions kept in the result
#name.group: names of the groups
#num.group.sup: indexes of the supplementary groups 


#-------------------------- Print the results ---------------------------------------
summary(res)

print.MFA
res$separate.analyses #Results of the PCA on each group of variable
res$eig

res$global.pca$var$cor
res$global.pca$quali.sup$v.test

plot.MFA

#-------------------------- Export ind coordonates ---------------------------------------
coor<- res$ind$`coord`
write.table(coor, "c:/coor_MFA_RSN.txt", sep="\t") 


