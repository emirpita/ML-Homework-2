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

#spojeni attrition setovi u jedan
attrition <- merge(attrition_train, attrition_test, by="X")
attrition <- rbind(attrition_train, attrition_test)
attrition$Attrition <- as.numeric(attrition$Attrition)
attrition <- subset(attrition, select=-c(BusinessTravel, Department, EducationField, Gender, JobRole, MaritalStatus, Over18, OverTime, BirthDate))

#kreiranje regresijskog modela
lm.fit<-lm(Attrition~.,data=attrition)
summary(lm.fit)

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

#Uklananje outliera
length(unique(which(abs(rstudent(lm.fit))>3)))
attrition1<-attrition[-unique(which(abs(rstudent(lm.fit))>3)),]

#Novi model
lm1.fit<-lm(Attrition~.,data=attrition1)
summary(lm1.fit)

#Detekcija leverage points
#OVDJE JE GRESKA U 55. LINIJI JE 0 PA BUDE U 56. PRAZAN SET
length(unique(which(hatvalues(lm1.fit)>10*(ncol(attrition1))/nrow(attrition1))))
#attrition1<-attrition1[-unique(which(hatvalues(lm1.fit)>10*(ncol(attrition1))/nrow(attrition1))),]

lm2.fit<-lm(Attrition~.,data=attrition1)
summary(lm2.fit)

#VIF
vif(lm2.fit)
#Remove column tax according to VIF
attrition2<-attrition1[,-10]

lm3.fit<-lm(Attrition~.,data=attrition2)
summary(lm3.fit)
vif(lm3.fit)