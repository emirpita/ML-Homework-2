library(ISLR)
library(e1071)
library(MASS)
library(klaR)
library(caret)
library(C50)
library(naivebayes)
library(pROC)

getwd()
setwd("E:/MU/Zadaca2/")

# Ucitavanje podataka (izvornih) u data frame
attrition_train<-read.csv("Data/attrition_train.csv", header=TRUE)

#Podjela seta podataka
n <- nrow(attrition_train);
eighty_percent <- floor(n * 0.8)
train_sample <- sample(1:n, eighty_percent) 
test_sample <- setdiff(1:n, train_sample) 

atrain <- attrition_train[train_sample, ] 
atest <- attrition_train[test_sample, ] 

atrain$Attrition <- as.factor(atrain$Attrition)
atest$Attrition <- as.factor(atest$Attrition)


#k-nn
#potrebno popuniti nedostajuce vrijednosti zbog predikcije
modelKnn <- knn3(Attrition~BusinessTravel+DailyRate+Department+DistanceFromHome+Education+EducationField+EmployeeNumber+
                EnvironmentSatisfaction+Gender+HourlyRate+JobInvolvement+JobLevel+JobRole+JobSatisfaction+MaritalStatus+
                MonthlyIncome+MonthlyRate+NumCompaniesWorked+OverTime+PercentSalaryHike+PerformanceRating+RelationshipSatisfaction+
                StockOptionLevel+TotalWorkingYears+TrainingTimesLastYear+WorkLifeBalance+YearsAtCompany+YearsInCurrentRole+
                YearsSinceLastPromotion+YearsWithCurrManager+BirthDate, data=atrain, na.action="na.omit")
modelKnn
predKnn <- predict(modelKnn, newdata = atest, type="class")
confusionMatrix(predKnn, atest$Attrition)
#prebacivanje u numericke vrijednosti zbog roc-a
#ROC -> https://rviews.rstudio.com/2019/03/01/some-r-packages-for-roc-curves/
predKnn_num <- as.numeric(predKnn)
attrition_test_num <- as.numeric(atest$Attrition)
lrROC <- roc(predKnn_num ~ attrition_test_num,plot=TRUE,print.auc=TRUE,col="green",lwd =4,legacy.axes=TRUE,main="ROC Curves")


#Naive Bayes
model_bayes <- naive_bayes(Attrition~., data=atrain, na.action="na.omit")
model_bayes
#izbacuje error kad se pokrene GlobalTransformation skripte pa onda ovo
pred_bayes <- predict(model_bayes, newdata = atest, type="class")
confusionMatrix(pred_bayes, atest$Attrition)
#ROC
pred_bayes_num <- as.numeric(pred_bayes)
attrition_test_num <- as.numeric(atest$Attrition)
lrROC <- roc(pred_bayes_num ~ attrition_test_num,plot=TRUE,print.auc=TRUE,col="green",lwd =4,legacy.axes=TRUE,main="ROC Curves")


#LDA
#izbacene varijable sa jednom vrijednosti i X sa vise njih
modelLDA <- lda(Attrition~BusinessTravel+DailyRate+Department+DistanceFromHome+Education+EducationField+EmployeeNumber+
                  EnvironmentSatisfaction+Gender+HourlyRate+JobInvolvement+JobLevel+JobRole+JobSatisfaction+MaritalStatus+
                  MonthlyIncome+MonthlyRate+NumCompaniesWorked+OverTime+PercentSalaryHike+PerformanceRating+RelationshipSatisfaction+
                  StockOptionLevel+TotalWorkingYears+TrainingTimesLastYear+WorkLifeBalance+YearsAtCompany+YearsInCurrentRole+
                  YearsSinceLastPromotion+YearsWithCurrManager+BirthDate, data=atrain, na.action="na.omit")
modelLDA
predLDA <- predict(modelLDA, newdata = atest)
predLDA_class <- predLDA$class
confusionMatrix(predLDA_class, atest$Attrition)
#ROC
predLDA_num <- as.numeric(predLDA_class)
attrition_test_num <- as.numeric(atest$Attrition)
lrROC <- roc(predLDA_num ~ attrition_test_num,plot=TRUE,print.auc=TRUE,col="green",lwd =4,legacy.axes=TRUE,main="ROC Curves")


#QDA
#JobRole, TrainingTimesLastYear i BirthDate - error zbog njih 
modelQDA <- qda(Attrition~BusinessTravel+DailyRate+Department+DistanceFromHome+Education+EducationField+EmployeeNumber+
                  EnvironmentSatisfaction+Gender+HourlyRate+JobInvolvement+JobLevel+JobSatisfaction+MaritalStatus+
                  MonthlyIncome+MonthlyRate+NumCompaniesWorked+OverTime+PercentSalaryHike+PerformanceRating+RelationshipSatisfaction+
                  StockOptionLevel+TotalWorkingYears+WorkLifeBalance+YearsAtCompany+YearsInCurrentRole+
                  YearsSinceLastPromotion+YearsWithCurrManager, data=atrain, na.action="na.omit")
modelQDA
predQDA <- predict(modelQDA, newdata = atest)
predQDA_class <- predQDA$class
confusionMatrix(predQDA_class, atest$Attrition)
#ROC
predQDA_num <- as.numeric(predQDA_class)
attrition_test_num <- as.numeric(atest$Attrition)
lrROC <- roc(predQDA_num ~ attrition_test_num,plot=TRUE,print.auc=TRUE,col="green",lwd =4,legacy.axes=TRUE,main="ROC Curves")


#logistièka regresija
model_logit <- glm(Attrition~BusinessTravel+DailyRate+Department+DistanceFromHome+Education+EducationField+EmployeeNumber+
               EnvironmentSatisfaction+Gender+HourlyRate+JobInvolvement+JobLevel+JobRole+JobSatisfaction+MaritalStatus+
               MonthlyIncome+MonthlyRate+NumCompaniesWorked+OverTime+PercentSalaryHike+PerformanceRating+RelationshipSatisfaction+
               StockOptionLevel+TotalWorkingYears+TrainingTimesLastYear+WorkLifeBalance+YearsAtCompany+YearsInCurrentRole+
               YearsSinceLastPromotion+YearsWithCurrManager+BirthDate, family=binomial(link='logit'), data=atrain, na.action="na.omit")
model_logit
pred_logitS <- predict(model_logit, newdata = atest)
pred_logit <- ifelse(pred_logitS > 0, 1, 0)
pred_logit_noname <- unname(pred_logit)
pred_logit_noname<-ifelse(pred_logit_noname==1,"Yes","No")
pred_logit_noname <- as.factor(pred_logit_noname)
confusionMatrix(pred_logit_noname, atest$Attrition)
#ROC
pred_logit_noname_num <- as.numeric(pred_logit_noname)
attrition_test_num <- as.numeric(atest$Attrition)
lrROC <- roc(pred_logit_noname_num ~ attrition_test_num,plot=TRUE,print.auc=TRUE,col="green",lwd =4,legacy.axes=TRUE,main="ROC Curves")



#SVM
svmfit = svm(Attrition~BusinessTravel+DailyRate+Department+DistanceFromHome+Education+EducationField+EmployeeNumber+
               EnvironmentSatisfaction+Gender+HourlyRate+JobInvolvement+JobLevel+JobRole+JobSatisfaction+MaritalStatus+
               MonthlyIncome+MonthlyRate+NumCompaniesWorked+OverTime+PercentSalaryHike+PerformanceRating+RelationshipSatisfaction+
               StockOptionLevel+TotalWorkingYears+TrainingTimesLastYear+WorkLifeBalance+YearsAtCompany+YearsInCurrentRole+
               YearsSinceLastPromotion+YearsWithCurrManager+BirthDate, data = atrain, kernel = "linear", cost = 10, scale = FALSE)
svmfit
pred_svmfit <- predict(svmfit, newdata = atest)
confusionMatrix(pred_svmfit, atest$Attrition)
#ROC
pred_svmfit_num <- as.numeric(pred_svmfit)
attrition_test_num <- as.numeric(atest$Attrition)
lrROC <- roc(pred_svmfit_num ~ attrition_test_num,plot=TRUE,print.auc=TRUE,col="green",lwd =4,legacy.axes=TRUE,main="ROC Curves")

