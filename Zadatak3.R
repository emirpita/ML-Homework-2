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
library(nnet)

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
skewness(cleanredWineData$fixed.acidity)
skewness(cleanredWineData$volatile.acidity)
skewness(cleanredWineData$citric.acid)
skewness(cleanredWineData$residual.sugar)
skewness(cleanredWineData$chlorides)
skewness(cleanredWineData$free.sulfur.dioxide)
skewness(cleanredWineData$total.sulfur.dioxide)
skewness(cleanredWineData$density)
skewness(cleanredWineData$pH)
skewness(cleanredWineData$sulphates)
skewness(cleanredWineData$alcohol)
skewness(cleanredWineData$quality)
skewness(cleanredWineData$HighQuality)
pairs(log(cleanredWineData))

# <----------------------------------------------------------------------------------->
# Izgradnja inicijalnog modela
# Koristit cemo logicku regresiju, iako se radi o klasifikacijskom problemu
# Varijabla HighQuality (kao i varijabla quality) su varijable sa 2 (tj 10) vrijednosti

redFit = lm(HighQuality~.-quality, cleanredWineData)
# redFit = lm(quality~.-HighQuality, cleanredWineData)
summary(redFit)
mean(redFit$residuals^2)
MAE(cleanredWineData$HighQuality,round(predict(redFit)))

# Izgradnja i pretrazivanje optimalnog modela koriteci logisticku regresiju
# <---------- METODA FORWARD ------------->
metrics = c("rsq", "rss", "adjr2", "cp", "bic")
summaryMetrics = NULL
rsRes.fwd = regsubsets(HighQuality~ .-quality, data=cleanredWineData, method = "forward", nvmax = 11)
summRes = summary(rsRes.fwd)
summRes$which
for (metricName in metrics) {
  summaryMetrics = rbind(summaryMetrics,
                             data.frame(method = "forward", metric = metricName,
                                    nvars = 1:length(summRes[[metricName]]),
                                    value = summRes[[metricName]]))
}
  
# Plotanje pripadnosti varijabli modelima
old.par = par(mfrow = c(1,2), ps = 16, mar = c(5,7,2,1))
image(1:nrow(summRes$which),
        1:ncol(summRes$which),
        summRes$which, xlab = 'N(vars)', ylab = '',
        xaxt = 'n', yaxt = 'n', breaks = c(-0.5, 0.5, 1.5),
        col = c('white', 'lightblue1'), main = "forward")
axis(1, 1:nrow(summRes$which), rownames(summRes$which))
axis(2, 1:ncol(summRes$which), colnames(summRes$which), las = 2)
par(old.par)

# Plotanje metrika modela
ggplot(summaryMetrics, aes(x = nvars, y = value, shape = method, colour = method)) + geom_path() + geom_point() + facet_wrap(~metric, scales = "free") + theme(legend.position = "top")

# <---------- METODA BACKWARD ------------->
summaryMetrics = NULL
rsRes.bwd = regsubsets(HighQuality~ .-quality, data=cleanredWineData, method = "backward", nvmax = 11)
summRes = summary(rsRes.bwd)
summRes$which
for (metricName in metrics) {
  summaryMetrics = rbind(summaryMetrics,
                         data.frame(method = "backward", metric = metricName,
                                    nvars = 1:length(summRes[[metricName]]),
                                    value = summRes[[metricName]]))
}

# Plotanje pripadnosti varijabli modelima
old.par = par(mfrow = c(1,2), ps = 16, mar = c(5,7,2,1))
image(1:nrow(summRes$which),
      1:ncol(summRes$which),
      summRes$which, xlab = 'N(vars)', ylab = '',
      xaxt = 'n', yaxt = 'n', breaks = c(-0.5, 0.5, 1.5),
      col = c('white', 'lightblue1'), main = "backward")
axis(1, 1:nrow(summRes$which), rownames(summRes$which))
axis(2, 1:ncol(summRes$which), colnames(summRes$which), las = 2)
par(old.par)

# Plotanje metrika modela
ggplot(summaryMetrics, aes(x = nvars, y = value, shape = method, colour = method)) + geom_path() + geom_point() + facet_wrap(~metric, scales = "free") + theme(legend.position = "top")

# Izgradnja optimalnog modela nakon sto smo zakljucili koje su znacajne varijable
redFit2 = lm(HighQuality~alcohol + sulphates + volatile.acidity + total.sulfur.dioxide + pH - quality, cleanredWineData)
summary(redFit2)
mean(redFit2$residuals^2)
MAE(cleanredWineData$HighQuality,round(predict(redFit2)))

# zakljucili smo da je najbolji model kad se sve varijable ukljuce
# isto kao i polazni
redFit3 = lm(HighQuality~.-quality, cleanredWineData)
summary(redFit3)
mean(redFit3$residuals^2)
MAE(cleanredWineData$HighQuality,round(predict(redFit3)))


# Predikcija
set.seed(5)
n <- nrow(cleanredWineData);
eighty_percent <- floor(n * 0.8)
train_sample <- sample(1:n, eighty_percent) 
test_sample <- setdiff(1:n, train_sample)
rwTrain <- cleanredWineData[train_sample, ] 
rwTest <- cleanredWineData[test_sample, ]

# Klasifikacija koristeci logisticku regresiju
model_logit <- glm(HighQuality~.-quality, family=binomial(link='logit'), data=rwTrain, na.action="na.omit")
model_logit
pred_logitS <- predict(model_logit, newdata = rwTest)
pred_logit <- ifelse(pred_logitS > 0, 1, 0)
pred_logit_noname <- unname(pred_logit)
pred_logit_noname<-ifelse(pred_logit_noname==1,"1","0")
pred_logit_noname <- as.factor(pred_logit_noname)
rwTest$HighQuality <- as.factor(rwTest$HighQuality)
rwTrain$HighQuality <- as.factor(rwTrain$HighQuality)
confusionMatrix(pred_logit_noname, rwTest$HighQuality, positive="1")

# Klasifikacija nad istim trening i testnim skupom koristeci neuronske mreze
rwNnet<-nnet(HighQuality~.-quality,data=rwTrain,size=4,decay=0.0001,maxit=500)
summary(rwNnet)
rwPredictions<-predict(rwNnet,rwTest[,-13],type="class")
rwPredictions<-ifelse(rwPredictions==1, 1, 0)
rwPredictions <- as.factor(rwPredictions)
confusionMatrix(rwPredictions, rwTest$HighQuality, positive="1")
