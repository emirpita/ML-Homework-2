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

#spojeni attrition setovi u jedan
attrition <- merge(attrition_train, attrition_test, by="X")
attrition <- rbind(attrition_train, attrition_test)
attrition$Attrition <- as.numeric(attrition$Attrition)
attrition <- subset(attrition, select=-c(X, EmployeeCount,PercentSalaryHike, StandardHours, TotalWorkingYears, YearsInCurrentRole, TrainingTimesLastYear, YearsSinceLastPromotion, Over18, JobLevel, JobRole, BirthDate))

consolidate.attrition <- function(attrition) {
  if(attrition == 'No') {
    return(0)
  } else if(attrition == 'Yes') {
    return(1)
  } else {
    return(attrition)
  }
}

#normalizacija trening seta
attrition$Attrition <- as.numeric(sapply(as.character(attrition$Attrition), consolidate.attrition, USE.NAMES=FALSE))

#odrediti neku selekciju koja ce mi reci koji je najbolji model ya predikciju
regfit.fwd = regsubsets(Attrition~.,data=attrition, nvmax = 30, method = "forward")
summary(regfit.fwd)
reg.summary = summary(regfit.fwd)
reg.summary$adjr2
regfit.bwd = regsubsets(Attrition~.,data=attrition, nvmax = 30, method = "backward")
summary(regfit.bwd)
reg.summary = summary(regfit.bwd)
reg.summary$adjr2

#kreiranje regresijskog modela
lm.fit<-lm(Attrition~.,data=attrition)
summary(lm.fit)
reg.summary = summary(lm.fit)
reg.summary$adj.r.squared

#posto plot nije htio iz prve evo popravka sa ove 2 linije, odkomentarisati ako nece
#par("mar")
#par(mar=c(1,1,1,1))

#Nelinearnost modela
plot(predict(lm.fit),residuals(lm.fit))
lines(smooth.spline(predict(lm.fit),residuals(lm.fit)), col="red")

#Auto-korelacija reziduala
plot(density(lm.fit$residuals) ,main="Residuals", xlab="Value")
plot(lm.fit, which=2)
shapiro.test(lm.fit$residuals)
dwtest(Attrition~.,data=attrition)

#Nekonstantna variansa reziduala
plot(predict(lm.fit),rstudent(lm.fit))
lines(smooth.spline(predict(lm.fit),rstudent(lm.fit)), col="red")

#Detekcija i uklanjanje outliera
length(unique(which(abs(rstudent(lm.fit))>3)))
if(length(unique(which(abs(rstudent(lm.fit))>3))) != 0) {
  attrition1<-attrition[-unique(which(abs(rstudent(lm.fit))>3)),]
}

#Novi model
lm1.fit<-lm(Attrition~.,data=attrition1)
summary(lm1.fit)
reg.summary = summary(lm1.fit)
reg.summary$adj.r.squared

#Detekcija i uklanjanje leverage points
length(unique(which(hatvalues(lm1.fit)>10*(ncol(attrition1))/nrow(attrition1))))
if(length(unique(which(hatvalues(lm1.fit)>10*(ncol(attrition1))/nrow(attrition1)))) != 0) {
  attrition1<-attrition1[-unique(which(hatvalues(lm1.fit)>10*(ncol(attrition1))/nrow(attrition1))),]
}

#Novi model
lm2.fit<-lm(Attrition~.,data=attrition1)
summary(lm2.fit)
reg.summary = summary(lm2.fit)
reg.summary$adj.r.squared

#VIF-kolinearnost
vif(lm2.fit)
#Remove column tax according to VIF
attrition2<-attrition1[,-10]

#Novi model
lm3.fit<-lm(Attrition~.,data=attrition2)
summary(lm3.fit)
reg.summary = summary(lm3.fit)
finalRsquared <- reg.summary$adj.r.squared
vif(lm3.fit)

#MAE
finalMAE <- MAE(attrition$Attrition,predict(lm3.fit))

#ispis
cat("Finalni adjusted R squared iznosi: ", finalRsquared, "\n")
cat("Finalni MAE iznosi: ", finalMAE)
