# CapStone R Project - Indian Liver Patient Records
# Author: Shweta Kothadiya
# Date: 03/12/2021

# Loading required packages
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(rpart)) install.packages("rpart", repos = "http://cran.us.r-project.org")
if(!require(randomForest)) install.packages("randomForest", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(data.table)
library(ggplot2)
library(rpart)
library(randomForest)

# CSV file "indian_liver_patient.csv" from my machine was read / pulled and saved the data in "indianLiverPatient" variable.
indianLiverPatient <- read.csv("C:\\Users\\Sanchit\\Documents\\Shweta\\edX\\Data Science Professional Certificate\\indian_liver_patient.csv", stringsAsFactors = FALSE)

# Check the first 6 lines of "indianLiverPatient" dataset. 
head(indianLiverPatient)

# Dimension of dataframe and class of dataframe and each column within.
dim(indianLiverPatient)
class(indianLiverPatient)
class(indianLiverPatient$Age)
class(indianLiverPatient$Gender)
class(indianLiverPatient$Total_Bilirubin)
class(indianLiverPatient$Direct_Bilirubin)
class(indianLiverPatient$Alkaline_Phosphotase)
class(indianLiverPatient$Alamine_Aminotransferase)
class(indianLiverPatient$Aspartate_Aminotransferase)
class(indianLiverPatient$Total_Protiens)
class(indianLiverPatient$Albumin)
class(indianLiverPatient$Albumin_and_Globulin_Ratio)
class(indianLiverPatient$Dataset)

# Get the count of records for Male and Female. 
indianLiverPatient %>% filter(Gender=="Male") %>% nrow()
indianLiverPatient %>% filter(Gender=="Female") %>% nrow()

# Patient with Liver disease and without liver disease count
indianLiverPatient %>% filter(Dataset=="1") %>% nrow()
indianLiverPatient %>% filter(Dataset=="2") %>% nrow()

# Let's check if any of the records has NA values.
indianLiverPatient[rowSums(is.na(indianLiverPatient))>0,]

# We found NA values under "Albumin_and_Globulin_Ratio".  
# Let's replace it with the mean value wherever NA values are found.
indianLiverPatient$Albumin_and_Globulin_Ratio=ifelse(is.na(indianLiverPatient$Albumin_and_Globulin_Ratio),ave(indianLiverPatient$Albumin_and_Globulin_Ratio,FUN = function(x)mean(x,na.rm = TRUE)),indianLiverPatient$Albumin_and_Globulin_Ratio)

# Let's check again if any NA values are still there.
indianLiverPatient[rowSums(is.na(indianLiverPatient))>0,]

# Setting Dataset and Gender columns as factor. Now "Female" will be shown as "0" and "Male" as "1".
ind_liv_patient_clean <- indianLiverPatient %>% 
  mutate(Dataset = factor(indianLiverPatient$Dataset), 
         Gender = factor(x=indianLiverPatient$Gender,levels = c('Female','Male'),labels=c(0,1))) %>% 
  select(Age, Gender, Total_Bilirubin, Direct_Bilirubin, Alkaline_Phosphotase, Alamine_Aminotransferase, Aspartate_Aminotransferase, Total_Protiens, Albumin, Albumin_and_Globulin_Ratio, Dataset)

# Check the first 6 records of this new cleaned data. Also check if the "Gender" and "Dataset" columns are set as "factor".  
head(ind_liv_patient_clean)
class(ind_liv_patient_clean$Gender)
class(ind_liv_patient_clean$Dataset)

# let's do some data analysis using plots on "ind_liv_patient_clean" data.
# Plot of Age Vs. Gender 
qplot(x=Age, y=Gender, data=ind_liv_patient_clean, geom="point")
# Plot of Age Vs. Dataset, grouped by Gender and colored by Gender. 
ind_liv_patient_clean %>% group_by(Gender) %>% ggplot(aes(x=Age, y =Dataset, color=Gender)) + geom_point()
# BoxPlot of Dataset Vs. Age,grouped by Gender and colored by Gender. 
ind_liv_patient_clean %>% group_by(Gender) %>% ggplot(aes(x=Dataset, y =Age, color=Gender)) + geom_boxplot()
# Boxplot of Dataset Vs. Total Bilirubin and grouped and colored by Gender
ind_liv_patient_clean %>% group_by(Gender) %>% ggplot(aes(x=Dataset, y =Total_Bilirubin, color=Gender)) + geom_boxplot()
# ggplot of Age Vs. Total Bilirubin and colored by Gender
ind_liv_patient_clean %>% ggplot(aes(Age,Total_Bilirubin, color=Gender)) + geom_point()
# ggplot of Gender and Total Bilirubin colored by Gender
ind_liv_patient_clean %>% ggplot(aes(Gender,Total_Bilirubin, color=Gender)) + geom_point()
# ggplot of Dataset and Alkaline Phosphotase colored by Gender
ind_liv_patient_clean %>% ggplot(aes(x=Dataset, y=Alkaline_Phosphotase, color=Gender)) + geom_point()
# Boxplot of Dataset Vs. Alkaline Phosphotase grouped and colored by Gender
ind_liv_patient_clean %>% group_by(Gender) %>% ggplot(aes(x=Dataset, y =Alkaline_Phosphotase, color=Gender)) + geom_boxplot()

# Setting seed to 1 with sample.kind as Rounding as I am using R version 4.0.3.
# Creating partition and sending 20 percent data into test set and rest under training.
set.seed(1, sample.kind = "Rounding")
test_index <- createDataPartition(ind_liv_patient_clean$Dataset, times = 1, p = 0.2, list = FALSE)
test_set <- ind_liv_patient_clean[test_index,]
train_set <- ind_liv_patient_clean[-test_index,]

# Check the dimentions on test and train set.
dim(test_set)
dim(train_set)

# Training LDA (Linear Discriminant Analysis) model on train_set to check if liver disease is affected by Total Bilirubin. 
# Using the train_lda model against test_set.
# Checking the accuracy between model's prediction and actual data in test_set using mean function.
train_lda <- train(Dataset ~ Total_Bilirubin, method = "lda", data = train_set)
lda_preds <- predict(train_lda, test_set)
mean(lda_preds == test_set$Dataset)

# Training QDA (Quadratic Discriminant Analysis) model on train_set to check if liver disease is affected by Total Bilirubin. 
# Using the train_qda model against test_set.
# Checking the accuracy between model's prediction and actual data in test_set using mean function.
train_qda <- train(Dataset ~ Total_Bilirubin, method = "qda", data = train_set)
qda_preds <- predict(train_qda, test_set)
mean(qda_preds == test_set$Dataset)

# Training LDA (Linear Discriminant Analysis) model on train_set to check if liver disease is affected by Total Bilirubin & Age combined.
# Using the train_lda_TB_Age model against test_set.
# Checking the accuracy between model's prediction and actual data in test_set using mean function.
train_lda_TB_Age <- train(Dataset ~ Total_Bilirubin + Age, method = "lda", data = train_set)
lda_preds_TB_Age <- predict(train_lda_TB_Age, test_set)
mean(lda_preds_TB_Age == test_set$Dataset)

# Training QDA (Quadratic Discriminant Analysis) model on train_set to check if liver disease is affected by Total Bilirubin & Age combined. 
# Using the train_qda_TB_Age model against test_set.
# Checking the accuracy between model's prediction and actual data in test_set using mean function.
train_qda_TB_Age <- train(Dataset ~ Total_Bilirubin + Age, method = "qda", data = train_set)
qda_preds_TB_Age <- predict(train_qda_TB_Age, test_set)
mean(qda_preds_TB_Age == test_set$Dataset)

# Training LDA (Linear Discriminant Analysis) model on train_set to check if liver disease is affected by Total Bilirubin & Gender combined.
# Using the train_lda_TB_gender model against test_set.
# Checking the accuracy between model's prediction and actual data in test_set using mean function.
train_lda_TB_gender <- train(Dataset ~ Total_Bilirubin + Gender, method = "lda", data = train_set)
lda_preds_TB_gender <- predict(train_lda_TB_gender, test_set)
mean(lda_preds_TB_gender == test_set$Dataset)

# Training QDA (Quadratic Discriminant Analysis) model on train_set to check if liver disease is affected by Total Bilirubin & Gender combined. 
# Using the train_qda_TB_gender model against test_set.
# Checking the accuracy between model's prediction and actual data in test_set using mean function.
train_qda_TB_gender <- train(Dataset ~ Total_Bilirubin + Gender, method = "qda", data = train_set)
qda_preds_TB_gender <- predict(train_qda_TB_gender, test_set)
mean(qda_preds_TB_gender == test_set$Dataset)

# Training LDA (Linear Discriminant Analysis) model on train_set to check if liver disease is affected by Age & Gender combined.
# Using the train_lda_Age_gender model against test_set.
# Checking the accuracy between model's prediction and actual data in test_set using mean function.
train_lda_Age_gender <- train(Dataset ~ Age + Gender, method = "lda", data = train_set)
lda_preds_Age_gender <- predict(train_lda_Age_gender, test_set)
mean(lda_preds_Age_gender == test_set$Dataset)

# Training QDA (Quadratic Discriminant Analysis) model on train_set to check if liver disease is affected by Age & Gender combined. 
# Using the train_qda_Age_gender model against test_set.
# Checking the accuracy between model's prediction and actual data in test_set using mean function.
train_qda_Age_gender <- train(Dataset ~ Age + Gender, method = "qda", data = train_set)
qda_preds_Age_gender <- predict(train_qda_Age_gender, test_set)
mean(qda_preds_Age_gender == test_set$Dataset)

# Training LDA (Linear Discriminant Analysis) model on train_set to check if liver disease is affected by Age.
# Using the train_lda_age model against test_set.
# Checking the accuracy between model's prediction and actual data in test_set using mean function.
train_lda_age <- train(Dataset ~ Age, method = "lda", data = train_set)
lda_preds_age <- predict(train_lda_age, test_set)
mean(lda_preds_age == test_set$Dataset)

# Training QDA (Quadratic Discriminant Analysis) model on train_set to check if liver disease is affected by Age. 
# Using the train_qda_age model against test_set.
# Checking the accuracy between model's prediction and actual data in test_set using mean function.
train_qda_age <- train(Dataset ~ Age, method = "qda", data = train_set)
qda_preds_age <- predict(train_qda_age, test_set)
mean(qda_preds_age == test_set$Dataset)

# Training GLM (Generalized Linear Model) model on train_set to check if liver disease is affected by Age. 
# Using the train_glm_age model against test_set.
# Checking the accuracy between model's prediction and actual data in test_set using mean function.
train_glm_age <- train(Dataset ~ Age, method = "glm", data = train_set)
glm_preds_age <- predict(train_glm_age, test_set)
mean(glm_preds_age == test_set$Dataset)

# Training GLM (Generalized Linear Model) model on train_set to check if liver disease is affected by Age, Gender, Direct Bilirubin combined. 
# Using the train_glm_age_gender_db model against test_set.
# Checking the accuracy between model's prediction and actual data in test_set using mean function.
train_glm_age_gender_db <- train(Dataset ~ Age + Gender + Direct_Bilirubin, method = "glm", data = train_set)
glm_preds_age_gender_db <- predict(train_glm_age_gender_db, test_set)
mean(glm_preds_age_gender_db == test_set$Dataset)

# Training GLM (Generalized Linear Model) model on train_set to check if liver disease is affected by all predictors combined. 
# Using the train_glm_all model against test_set.
# Checking the accuracy between model's prediction and actual data in test_set using mean function.
train_glm_all <- train(Dataset ~ ., method = "glm", data = train_set)
glm_preds_all <- predict(train_glm_all, test_set)
mean(glm_preds_all == test_set$Dataset)

# Training LDA (Linear Discriminant Analysis) model on train_set to check if liver disease is affected by Alkaline_Phosphotase.
# Using the train_lda_AlkPho model against test_set.
# Checking the accuracy between model's prediction and actual data in test_set using mean function.
train_lda_AlkPho <- train(Dataset ~ Alkaline_Phosphotase, method = "lda", data = train_set) 
lda_preds_AlkPho <- predict(train_lda_AlkPho, test_set)
mean(lda_preds_AlkPho == test_set$Dataset)

# Training QDA (Quadratic Discriminant Analysis) model on train_set to check if liver disease is affected by Alkaline_Phosphotase. 
# Using the train_qda_AlkPho model against test_set.
# Checking the accuracy between model's prediction and actual data in test_set using mean function.
train_qda_AlkPho <- train(Dataset ~ Alkaline_Phosphotase, method = "qda", data = train_set)
qda_preds_AlkPho <- predict(train_qda_AlkPho, test_set)
mean(qda_preds_AlkPho == test_set$Dataset)

# Training GLM (Generalized Linear Model) model on train_set to check if liver disease is affected by Alkaline_Phosphotase. 
# Using the train_glm_AlkPho model against test_set.
# Checking the accuracy between model's prediction and actual data in test_set using mean function.
train_glm_AlkPho <- train(Dataset ~ Alkaline_Phosphotase, method = "glm", data = train_set)
glm_preds_AlkPho <- predict(train_glm_AlkPho, test_set)
mean(glm_preds_AlkPho == test_set$Dataset)

# Training Classification Tree model on train_set to check if liver disease is affected by all the predictors combined. 
# Using the train_rpart model against test_set.
# Checking the accuracy between model's prediction and actual data in test_set using mean function.
train_rpart <- train(Dataset ~ ., method = "rpart", tuneGrid = data.frame(cp = seq(0, 0.05, 0.002)), data = train_set)
plot(train_rpart)
rpart_preds <- predict(train_rpart, test_set)
mean(rpart_preds == test_set$Dataset)

train_rpart$bestTune
train_rpart$finalModel
train_rpart

# Training RF (Random Forest Model) model on train_set to check if liver disease is affected by all the predictors combined. 
# Using the train_rf model against test_set.
# Checking the accuracy between model's prediction and actual data in test_set using mean function.
train_rf <- train(Dataset ~ ., data = train_set, method = "rf", ntree = 100, tuneGrid = data.frame(mtry = seq(1:7)))
rf_preds <- predict(train_rf, test_set)
mean(rf_preds == test_set$Dataset)

# Plot train_rf model
ggplot(train_rf)
plot(train_rf)

# Let's calculate variable importance on train_rf
varImp(train_rf)

# Random Forest model with nodesize as 50 and maxnodes as 25.
# Plot train_rf_n model
# Using the train_rf_n model against test_set.
# Checking the accuracy between model's prediction and actual data in test_set using mean function.
train_rf_n <- train(Dataset ~ ., data = train_set, method = "rf", nodesize = 50, maxnodes = 25)
plot(train_rf_n)
rf_preds_n <- predict(train_rf_n, test_set)
mean(rf_preds_n == test_set$Dataset)

# Let's calculate variable importance on train_rf_n
varImp(train_rf_n)





