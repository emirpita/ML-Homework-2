library(leaps)
library(ggplot2)
library(reshape2)
library(MASS)
library(ggcorrplot)
library(plotmo)

# Ucitavanje podataka
redWineData = read.table("Data/winequality-red.csv", sep = ";", header = T)

# Funkcija za sortiranje kolona dataseta u odnosu na korelaciju sa outcome (korelaciju sa Quality kolonom)
sortByCorr = function(dataset, refColName) {
  # Sort the dataframe columns by the absolute value of their correlation with
  # a given column
  #
  # Args:
  #   dataset: A vector, matrix, or data frame to sort
  #   refColName: The name of the reference colum for the correlation
  #
  # Returns:
  #   The sorted dataframe
  refColIdx = grep(refColName, colnames(dataset))
  corrTmp = cor(dataset)[, refColIdx]
  corrTmp[order(abs(corrTmp), decreasing = TRUE)]
  
  dataset[, order(abs(corrTmp), decreasing = TRUE)]
}


# Analiza podataka
dim(redWineData)
sapply(redWineData, class)
summary(redWineData)

# Prikaz box plotova za analizu outliers
oldpar = par(mfrow = c(2,6))
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
coutliers = as.numeric(rownames(redWineData[cooksd > 4 * mean(cooksd, na.rm=T), ]))
redOutliers = c(redOutliers , coutliers[ !coutliers %in% redOutliers ] )

cleanredWineData = redWineData[-redOutliers, ]
oldpar = par(mfrow=c(6,2))

for ( i in 1:12 ) {
  truehist(cleanredWineData[[i]], xlab = names(cleanredWineData)[i], col = 'lightgreen', main = paste("Average =", signif(mean(cleanredWineData[[i]]),3)), nbins = 50)
}
par(oldpar)

# Analiza korelacije

# S obzirom da su sve numericke varijable, lako je analizirati korelaciju
# U ovom dijelu, mi cemo plottati korelaciju koristeci se funkcijom iz biblioteke ggplot2
ggcorrplot(cor(cleanredWineData), hc.order = TRUE, type = "lower", lab = TRUE, insig = "blank")

# Prikaz varijabli sa najvecom korelacijom
colnames(sortByCorr(dataset = cleanredWineData, refColName = 'quality'))

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
pairs(log(cleanredWineData))