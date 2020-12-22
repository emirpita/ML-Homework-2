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

# Ucitavanje podataka (izvornih) u data frame
attrition_train<-read.csv("Data/attrition_train.csv", header=TRUE)
attrition_test<-read.csv("Data/attrition_test.csv", header=TRUE)
attrition_test$Attrition <- as.factor(attrition_test$Attrition)
#oversampling
#attrition_train<-ovun.sample(Attrition~., data = attrition_train, method="over", N=10000)$data
#undersampling
#attrition_train<-ovun.sample(Attrition~., data = attrition_train, method="under", N=400, seed = 1)$data


# Postavljanje faktor varijabli
attrition_train$Attrition <- as.factor(attrition_train$Attrition)
#prebacivanje BirthDate varijable u Date tip i agregiranje na godinu rodjenja
attrition_train$BirthDate <- as.Date(attrition_train$BirthDate)
attrition_test$BirthDate <- as.Date(attrition_test$BirthDate)

#attrition_train$BirthDate <- format(attrition_train$BirthDate, "%Y")


# Kratka analiza podataka
names(attrition_train)
dim(attrition_train)
summary(attrition_train)


str(attrition_train)


# Provjera nedostajucih vrijednosti
md.pattern(attrition_train)
aggr(attrition_train)


#provjera koje varijable imaju prazne redove
colnames(attrition_train)[colSums(is.na(attrition_train)) > 0]

#provjera tipova tih varijabli
typeof(attrition_train$Department)
typeof(attrition_train$EducationField)
typeof(attrition_train$JobRole)
typeof(attrition_train$Over18)
typeof(attrition_train$PercentSalaryHike)
typeof(attrition_train$StandardHours)
typeof(attrition_train$TrainingTimesLastYear)

#popunjavamo ih sa mode vrijednostima(možemo staviti i neku drugu funkciju)
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

attrition_train[is.na(attrition_train$Department),'Department']<-getmode(attrition_train[!is.na(attrition_train$Department),'Department'])
attrition_train[is.na(attrition_train$EducationField),'EducationField']<-getmode(attrition_train[!is.na(attrition_train$EducationField),'EducationField'])
attrition_train[is.na(attrition_train$JobRole),'JobRole']<-getmode(attrition_train[!is.na(attrition_train$JobRole),'JobRole'])
attrition_train[is.na(attrition_train$Over18),'Over18']<-getmode(attrition_train[!is.na(attrition_train$Over18),'Over18'])
attrition_train[is.na(attrition_train$PercentSalaryHike),'PercentSalaryHike']<-getmode(attrition_train[!is.na(attrition_train$PercentSalaryHike),'PercentSalaryHike'])
attrition_train[is.na(attrition_train$StandardHours),'StandardHours']<-getmode(attrition_train[!is.na(attrition_train$StandardHours),'StandardHours'])
attrition_train[is.na(attrition_train$TrainingTimesLastYear),'TrainingTimesLastYear']<-getmode(attrition_train[!is.na(attrition_train$TrainingTimesLastYear),'TrainingTimesLastYear'])

consolidate.education <- function(education) {
  if(education == 'Below College') {
    return(1)
  } else if(education == 'College') {
    return(2)
  } else if(education == 'Bachelor') {
    return(3)
  } else if(education == 'Master') {
    return(4)
  } else if(education == 'Doctor') {
    return(5)
  } else {
    return(education)
  }
}
consolidate.jobLevel <- function(jobLevel) {
  if(jobLevel == 'One') {
    return(1)
  } else if(jobLevel == 'Two') {
    return(2)
  } else if(jobLevel == 'Three') {
    return(3)
  } else if(jobLevel == 'Four') {
    return(4)
  } else if(jobLevel == 'Five') {
    return(5)
  } else {
    return(jobLevel)
  }
}
consolidate.enviromentSatisfaction <- function(eSatisfaction) {
  if(eSatisfaction == 'Low') {
    return(1)
  } else if(eSatisfaction == 'Medium') {
    return(2)
  } else if(eSatisfaction == 'High') {
    return(3)
  } else if(eSatisfaction == 'Very High') {
    return(4)
  }else {
    return(eSatisfaction)
  }
}
consolidate.jobInvolvment <- function(jobInvolvment) {
  if(jobInvolvment == 'Low') {
    return(1)
  } else if(jobInvolvment == 'Medium') {
    return(2)
  } else if(jobInvolvment == 'High') {
    return(3)
  } else if(jobInvolvment == 'Very High') {
    return(4)
  } else {
    return(jobInvolvment)
  }
}
consolidate.workLifeBalance <- function(workLifeBalance) {
  if(workLifeBalance == 'Bad') {
    return(1)
  } else if(workLifeBalance == 'Good') {
    return(2)
  } else if(workLifeBalance == 'Better') {
    return(3)
  } else if(workLifeBalance == 'Best') {
    return(4)
  } else {
    return(workLifeBalance)
  }
}

consolidate.relationshipSatisfaction <- function(relationshipSatisfaction) {
  if(relationshipSatisfaction == 'Low') {
    return(1)
  } else if(relationshipSatisfaction == 'Medium') {
    return(2)
  } else if(relationshipSatisfaction == 'High') {
    return(3)
  } else if(relationshipSatisfaction == 'Very High') {
    return(4)
  } else {
    return(relationshipSatisfaction)
  }
}

consolidate.performanceRating <- function(performanceRating) {
  if(performanceRating == 'Low') {
    return(1)
  } else if(performanceRating == 'Good') {
    return(2)
  } else if(performanceRating == 'Excellent') {
    return(3)
  } else if(performanceRating == 'Outstanding') {
    return(4)
  } else {
    return(performanceRating)
  }
}

consolidate.jobSatisfaction <- function(jobSatisfaction) {
  if(jobSatisfaction == 'Low') {
    return(1)
  } else if(jobSatisfaction == 'Medium') {
    return(2)
  } else if(jobSatisfaction == 'High') {
    return(3)
  } else if(jobSatisfaction == 'Very High') {
    return(4)
  } else {
    return(jobSatisfaction)
  }
}
consolidate.stockoptionlevel <- function(solevel) {
  if(solevel == 'One') {
    return(1)
  } else if(solevel == 'Zero') {
    return(0)
  } else {
    return(solevel)
  }
}
#normalizacija trening seta
attrition_train$Education <- as.numeric(sapply(as.character(attrition_train$Education), consolidate.education, USE.NAMES=FALSE))
attrition_train$EnvironmentSatisfaction <- as.numeric(sapply(as.character(attrition_train$EnvironmentSatisfaction), consolidate.enviromentSatisfaction, USE.NAMES=FALSE))
attrition_train$JobInvolvement <- as.numeric(sapply(as.character(attrition_train$JobInvolvement), consolidate.jobInvolvment, USE.NAMES=FALSE))
attrition_train$JobSatisfaction <- as.numeric(sapply(as.character(attrition_train$JobSatisfaction), consolidate.jobSatisfaction, USE.NAMES=FALSE))
attrition_train$PerformanceRating <- as.numeric(sapply(as.character(attrition_train$PerformanceRating), consolidate.performanceRating, USE.NAMES=FALSE))
attrition_train$RelationshipSatisfaction <- as.numeric(sapply(as.character(attrition_train$RelationshipSatisfaction), consolidate.relationshipSatisfaction, USE.NAMES=FALSE))
attrition_train$WorkLifeBalance <- as.numeric(sapply(as.character(attrition_train$WorkLifeBalance), consolidate.workLifeBalance, USE.NAMES=FALSE))
attrition_train$JobLevel <- as.numeric(sapply(as.character(attrition_train$JobLevel), consolidate.jobLevel, USE.NAMES=FALSE))
attrition_train$StockOptionLevel <- as.numeric(sapply(as.character(attrition_train$StockOptionLevel), consolidate.stockoptionlevel, USE.NAMES=FALSE))

attrition_test$Education <- as.numeric(sapply(as.character(attrition_test$Education), consolidate.education, USE.NAMES=FALSE))
attrition_test$EnvironmentSatisfaction <- as.numeric(sapply(as.character(attrition_test$EnvironmentSatisfaction), consolidate.enviromentSatisfaction, USE.NAMES=FALSE))
attrition_test$JobInvolvement <- as.numeric(sapply(as.character(attrition_test$JobInvolvement), consolidate.jobInvolvment, USE.NAMES=FALSE))
attrition_test$JobSatisfaction <- as.numeric(sapply(as.character(attrition_test$JobSatisfaction), consolidate.jobSatisfaction, USE.NAMES=FALSE))
attrition_test$PerformanceRating <- as.numeric(sapply(as.character(attrition_test$PerformanceRating), consolidate.performanceRating, USE.NAMES=FALSE))
attrition_test$RelationshipSatisfaction <- as.numeric(sapply(as.character(attrition_test$RelationshipSatisfaction), consolidate.relationshipSatisfaction, USE.NAMES=FALSE))
attrition_test$WorkLifeBalance <- as.numeric(sapply(as.character(attrition_test$WorkLifeBalance), consolidate.workLifeBalance, USE.NAMES=FALSE))
attrition_test$JobLevel <- as.numeric(sapply(as.character(attrition_test$JobLevel), consolidate.jobLevel, USE.NAMES=FALSE))
attrition_test$StockOptionLevel <- as.numeric(sapply(as.character(attrition_test$StockOptionLevel), consolidate.stockoptionlevel, USE.NAMES=FALSE))




#funkcije za transformaciju i skaliranje varijabli
# min-max
min_max<-function(x){
  transformed_x<-(x-min(x))/(max(x)-min(x))
}
# z-score transformation
z_score<-function(x){
  z<-(x-mean(x))/sd(x)
}

# decimal scaling
decimal<-function(x){
  max_x<-max(x)
  max_x<-as.character(max_x)
  j<-nchar(max_x)
  y<-x/(10^j)
  return(y)
}

attrition_train[,c(4,10,20)]<-lapply(attrition_train[,c(4,10,20)],z_score)
#primjena log funkcije na varijable koje su smaknute
attrition_train$HourlyRate<-log(attrition_train$HourlyRate)
attrition_train$MonthlyIncome<-log(attrition_train$MonthlyIncome)

attrition_test[,c(4,10,20)]<-lapply(attrition_test[,c(4,10,20)],z_score)
#primjena log funkcije na varijable koje su smaknute
attrition_test$HourlyRate<-log(attrition_test$HourlyRate)
attrition_test$MonthlyIncome<-log(attrition_test$MonthlyIncome)
