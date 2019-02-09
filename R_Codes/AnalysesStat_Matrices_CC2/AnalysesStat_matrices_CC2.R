library(xlsx)

# call the DATA file, datas in the first sheet
DFx <- read.xlsx("CC2_37.xls",1)

# check data format, and transform PPS in integer
str(DFx)  # provides a concise description of the structure of DFX
DFx$pps <- as.integer(DFx$pps)

# create a new variable : mean score of Cog (i.e. Besoin de cognition)
DFx <- within(DFx, {Cog <- rowMeans( cbind(nfc1, nfc2, -nfc3, -nfc4, -nfc5, nfc6, -nfc7, -nfc8, -nfc9, nfc10, nfc11, -nfc12, nfc13, nfc14, nfc15, -nfc16, -nfc17, nfc18), na.rm=TRUE)})

# item name ,item number, nvalid, mean, sd, median, mad, min, max, skew, kurtosis, se
library(psych)
describe(DFx)
describeBy(DFx, DFx$parf)

# normality test KS
ks.test(Cog, "pnorm", mean(Cog), sd(Cog))
ks.test(cert, "pnorm", mean(cert), sd(cert))

#-------------------------- Descriptive stats ---------------------------------------

# summary of the datas
summary(DFx)

# to obtain the mean of CERT depending on the value of the categorial variable PARF
tapply(DFx$cert, DFx$parf, mean)
boxplot(DFx$cert~DFx$parf)

#-------------------------- Inferential stats -------------------------------------

# regression with PARF as a predictor and CERT as dependant variable
fit1 <- lm(cert ~ parf,DFx)

# results
summary(fit1); anova(fit1)

# graph and regression line
plot(DFx$parf, DFx$cert, xlab="parfum", ylab="certitude", main="Effet du parfum sur l'embauche")
abline(fit1, col="red")

#--------------------------
# regression with PARF and COG as predictors and CERT as dependant variable
fit2 <- aov(cert ~ parf * Cog, DFx) 

# results
summary(fit2)

# graph and regression line
# Two-way Interaction Plot
attach(DFx)
parf <- factor(parf)
interaction.plot(Cog, parf, cert, type="p", col=c(1:3),
                 leg.bty="o", leg.bg="beige", lwd=2, pch=c(18,24,22),
                 xlab="Besoin de cognition",
                 ylab="Certitude � l'embauche",
                 main="Interaction Plot")
library(gplots)
attach(DFx)
parf <- factor(parf)
plotmeans(cert ~ parf,xlab="Parfum",ylab="Certitude � l'embauche", main="Mean Plot\nwith 95% CI") 

#-------------------------- PCA ---------------------------------------

# Pricipal Components Analysis, creates matrix with the items, and extracts PCs from the correlation matrix
attach(DFx)
Nfc <- as.matrix(cbind(nfc1, nfc2, -nfc3, -nfc4, -nfc5, nfc6, -nfc7, -nfc8, -nfc9, nfc10, nfc11, -nfc12, nfc13, nfc14, nfc15, -nfc16, -nfc17, nfc18))
Nfc <- as.data.frame(Nfc)
fit3 <- princomp(Nfc, cor = FALSE, scores = TRUE)

summary(fit3) # print variance accounted for
loadings(fit3) # factor loadings value (from -1 to +1; or "saturation de la variable sur le facteur") 
# a variable is associated to a factor if the absolute value of the loading is above 0.30 

plot(fit3,type="lines") # scree plot
fit3$scores # 
biplot(fit3) 