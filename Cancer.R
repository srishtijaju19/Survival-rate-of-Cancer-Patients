# Load data ---------------------------------------------------------------
library(forecast)
library(corrplot)
library(tokenizers)
library(tidytext)
library(dplyr)
library(tidyr)
library(reshape)
library(ggplot2)
library(qdapTools)
library(randomForest)
train_data <- read.csv("training_data.csv")
test_data <- read.csv("Srishti_score.csv")

sapply(test_data,function(x) sum(is.na(x)))

filteredtrain_data <- train_data[, -c(1,2,10,16,18,19,21,22,23)]
filteredtest_data <- test_data[, -c(1,2,10,16,18,19,21,22,23)]

# interaction of variables---------------------------------------------------------------------

t.test(train_data$tea~train_data$survival_7_years)

chisq<-chisq.test(train_data$survival_7_years,train_data$family_history)
chisq
chisq$observed

barplot(chisq$observed, beside=TRUE)

# correlation between tumor and psa-------------------------------------------------------------

num_data <- train_data[, c(20,21,22)]
View(num_data)
cormat <- cor(num_data)
corrplot(cormat, method="number", type="upper")


# Remove missing values ---------------------------------------------------

complete.cases(filteredtrain_data)
filteredtrain_data <- filteredtrain_data[complete.cases(filteredtrain_data),]

sapply(filteredtrain_data,function(x) sum(is.na(x)))

# transform variables -----------------------------------------------------
#creating bins for age in train
filteredtrain_data$age [filteredtrain_data$age<=60] <- '30-60'
filteredtrain_data$age [filteredtrain_data$age>60 & filteredtrain_data$age<=70] <- '60-70'
filteredtrain_data$age [filteredtrain_data$age>70 & filteredtrain_data$age<=80] <- '70-80'
filteredtrain_data$age [filteredtrain_data$age>80] <- '80-100'
table(filteredtrain_data$age)

#creating bins for age in test
filteredtest_data$age [filteredtest_data$age<=60] <- '30-60'
filteredtest_data$age [filteredtest_data$age>60 & filteredtest_data$age<=70] <- '60-70'
filteredtest_data$age [filteredtest_data$age>70 & filteredtest_data$age<=80] <- '70-80'
filteredtest_data$age [filteredtest_data$age>80] <- '80-100'
table(filteredtest_data$age)

#filteredtrain_data$weight [filteredtrain_data$weight<=150] <- '<150'
#filteredtrain_data$weight [filteredtrain_data$weight>150 & filteredtrain_data$weight<=170] <- '150-170'
#filteredtrain_data$weight [filteredtrain_data$weight>170 & filteredtrain_data$weight<=180] <- '170-180'
#filteredtrain_data$weight [filteredtrain_data$weight>180 & filteredtrain_data$weight<=200] <- '180-200'
#filteredtrain_data$weight [filteredtrain_data$weight>200 & filteredtrain_data$weight<=250] <- '200-250'
#filteredtrain_data$weight [filteredtrain_data$weight>250] <- '>250'
#table(filteredtrain_data$weight)



# One - hot encoding 
#Race
#for (i in sort(unique(filteredtrain_data$race))){
#  filteredtrain_data[i] <-ifelse(filteredtrain_data$race == i, 1,0)
#  colnames(filteredtrain_data)[which(names(filteredtrain_data) == i)] <- paste("RaceGrp_",i,sep = "")
#}

for(unique_value in unique(filteredtrain_data$race)){
    filteredtrain_data[paste("Racegrp_", unique_value, sep = "")] <- ifelse(filteredtrain_data$race == unique_value, 1, 0)
}

for(unique_value in unique(filteredtest_data$race)){
  filteredtest_data[paste("Racegrp_", unique_value, sep = "")] <- ifelse(filteredtest_data$race == unique_value, 1, 0)
}


# Tokenize symptom --------------------------------------------------------

filteredtrain_data <- cbind(filteredtrain_data, mtabulate(strsplit(as.character(filteredtrain_data$symptoms), ",")))
View(filteredtrain_data)
filteredtrain_data <- filteredtrain_data[, -c(15)]
filteredtrain_data <- filteredtrain_data[, -c(7)]

filteredtest_data <- cbind(filteredtest_data, mtabulate(strsplit(as.character(filteredtest_data$symptoms), ",")))
View(filteredtest_data)

filteredtest_data <- filteredtest_data[, -c(15)]
filteredtest_data <- filteredtest_data[, -c(7)]


# Logistic model ----------------------------------------------------------
logistic_model <- glm(formula = survival_7_years ~ .,family = binomial(link='logit'),data=filteredtrain_data)
summary(logistic_model)

#Prediction Trainset
pred <- predict(logistic_model, filteredtrain_data, type = 'response')
pred1 <- ifelse(pred > 0.5,1,0)
View(pred1)
View(filteredtest_data)
sum(is.na(filteredtest_data$survival_7_years))

#Confusion Matrix
cnf<-table(filteredtrain_data$survival_7_years,pred1)
cnf

#Accuracy
accuracy <- sum(diag(cnf))/sum(cnf) *100
print (accuracy)

# Recall
recall <- cnf[2,2]/sum(cnf[2,]) *100
print(recall)


#Precision
precision <- cnf[2,2]/sum(cnf[,2]) *100
print(precision)



# Decision Tree -----------------------------------------------------------
library(rpart)
model1 <- rpart(filteredtrain_data$survival_7_years ~ ., data = filteredtrain_data, method = 'class', parms = list(split = "information"))
model1

#Prediction Trainset
pred1 <- predict(model1, filteredtest_data, type = 'vector', na.action = na.rpart) >= 0.5
pred1 <- ifelse(pred > 0.5,1,0)
View(pred1)

write.csv(pred1,file="Predicted_test_scores",row.names = FALSE)

#Confusion Matrix
cnf1<-table(filteredtrain_data$survival_7_years,pred1)
cnf1

#Accuracy
accuracy <- sum(diag(cnf1))/sum(cnf1) *100
print (accuracy)

