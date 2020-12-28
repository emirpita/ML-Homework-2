library(leaps)
library(ggplot2)
library(reshape2)
library(MASS)
library(ggcorrplot)
library(plotmo)
library(e1071)
library(MASS)
library(caret)
library(rpart)
library(mice)
library(VIM)
library(ggplot2)
library(statip)
library(e1071)
library(ISLR)
library(tree)
library(Hmisc)
library(descr)
library(lmtest)
library(lattice)
library(car)
library(leaps)

# Ucitavanje podataka
redWineData = read.table("Data/winequality-red.csv", sep = ";", header = T)
redWineData$HighQuality = ifelse(redWineData$quality <=5,0,1)
redWineData$HighQuality = as.integer(redWineData$HighQuality)
redWineData$quality = as.integer(redWineData$quality)
#HighQuality = ifelse(redWineData$quality <=5,"No","Yes")
#redWineData<-data.frame(redWineData, HighQuality)

# Analiza podataka
dim(redWineData)
sapply(redWineData, class)
summary(redWineData)

# Prikaz box plotova za analizu outliers
oldpar = par(mfrow = c(2,6))
#posto plot nije htio iz prve evo popravka sa ove 2 linije, odkomentarisati ako nece
par("mar")
par(mar=c(1,1,1,1))
for ( i in 1:11 ) {
  boxplot(redWineData[[i]])
  mtext(names(redWineData)[i], cex = 0.8, side = 1, line = 2)
}
par(oldpar) #za postavljanje grafickih parametara

# Pregled histograma (POOS) za analizu distribucije
oldpar = par(mfrow = c(6,2))
for ( i in 1:12 ) {
  truehist(redWineData[[i]], xlab = names(redWineData)[i], col = 'lightgreen', main = paste("Average =", signif(mean(redWineData[[i]]),3)), nbins = 50)
}
par(oldpar)


# Analiza lokacija outliers-a
outliers = c()
for ( i in 1:11 ) {
  stats = boxplot.stats(redWineData[[i]])$stats
  bottom_outlier_rows = which(redWineData[[i]] < stats[1])
  top_outlier_rows = which(redWineData[[i]] > stats[5])
  outliers = c(outliers , top_outlier_rows[ !top_outlier_rows %in% outliers ] )
  outliers = c(outliers , bottom_outlier_rows[ !bottom_outlier_rows %in% outliers ] )
}


# Analiza distribucije varijabli nakon uklanjanja outliers-a

cleanredWineData = redWineData[-outliers, ]
oldpar = par(mfrow=c(6,2))

for ( i in 1:12 ) {
  truehist(cleanredWineData[[i]], xlab = names(cleanredWineData)[i], col = 'lightgreen', main = paste("Average =", signif(mean(cleanredWineData[[i]]),3)), nbins = 50)
}
par(oldpar)

# Analiza korelacije

# S obzirom da su sve numericke varijable, lako je analizirati korelaciju
# U ovom dijelu, mi cemo plottati korelaciju koristeci se funkcijom iz biblioteke ggplot2
ggcorrplot(cor(cleanredWineData), hc.order = TRUE, type = "lower", lab = TRUE, insig = "blank",  colors = c("purple", "white", "yellow"))

# Prikaz varijabli sa najvecom korelacijom

# Funkcija za sortiranje kolona dataseta u odnosu na korelaciju

# Sortira kolone u dataframe u odnosu na apsolutnu vrijednost
# njihove korelacije sa zadanom kolonom
# Kao argumente uzima ataset - vektor, matrica ili data frame za sortiranje,
# referentnaKolona - naziv referentne kolone u odnosu na koju sortiramo
# Povratna vrijednost: dataset sortiran pomocu funkcije order

sortByCorr = function(dataset, referentaKolona) {
  indeksRefKolone = grep(referentaKolona, colnames(dataset))
  pomocnaCorr = cor(dataset)[, indeksRefKolone]
  pomocnaCorr[order(abs(pomocnaCorr), decreasing = TRUE)]
  
  dataset[, order(abs(pomocnaCorr), decreasing = TRUE)]
}

colnames(sortByCorr(dataset = cleanredWineData, referentaKolona = 'quality'))

# Provjera smaknutosti varijabli

# Smaknutost cemo provjeriti pomocu funkcije skewness
# Nakon toga, provjeravamo bi li logaritamska transformacija poboljsala
# linearnost izmedju prediktora i izlaza
skewness(redWineData$fixed.acidity)
skewness(redWineData$volatile.acidity)
skewness(redWineData$citric.acid)
skewness(redWineData$residual.sugar)
skewness(redWineData$chlorides)
skewness(redWineData$free.sulfur.dioxide)
skewness(redWineData$total.sulfur.dioxide)
skewness(redWineData$density)
skewness(redWineData$pH)
skewness(redWineData$sulphates)
skewness(redWineData$alcohol)
skewness(redWineData$quality)
skewness(redWineData$HighQuality)
pairs(log(cleanredWineData))

# --------------------------------------------
# Izgradnja inicijalnog modela
# Koristit cemo logicku regresiju, iako se radi o klasifikacijskom problemu
# Varijabla HighQuality (kao i varijabla quality) su varijable sa 2 (tj 10) vrijednosti

redFit = lm(HighQuality~.-quality, cleanredWineData)
# redFit = lm(quality~.-HighQuality, cleanredWineData)
summary(redFit)
mean(redFit$residuals^2)
MAE(cleanredWineData$HighQuality,round(predict(redFit)))

# Izgradnja i pretrazivanje optimalnog modela koriteci logisticku regresiju
rsRes.fwd = regsubsets(HighQuality~.-quality, data=cleanredWineData, method = "forward", nvmax = 11)
summRes = summary(rsRes.fwd)
summRes$adjr2
MAE(cleanredWineData$HighQuality,round(predict(rsRes.fwd)))
  
  
  
  

