setwd("/Users/stephenreagin/Desktop/ADS503_project/ADS503")

main_data <- read.csv('risk_factors_cervical_cancer.csv')

main_data
library(MASS)
library(dplyr)
library(caret)

#checking for zero variance predictors
data_nearzero <- as.integer(nearZeroVar(main_data))



# check for missing data
data_miss <- sapply(main_data, function(x) sum(is.na(x)))

q_idx <- as.data.frame(unlist(which(main_data == "?", arr.ind = TRUE)))

#rows with missing "?" data points
# note this is NOT a total count of "?" points
# but a count of whether or not a given row has AT LEAST ONE "?"
rows <- unique(unlist(q_idx[1]))

#dataframe of all observations with at least one missing value
data_test <- main_data[c(rows),]

#replacing the "?" with NA values
main_data[main_data == "?"] <- NA

data_wstd <- main_data


summary(as.integer(main_data$Num.of.pregnancies ))

test <- main_data$Num.of.pregnancies == "0.0"
summary(test)


#negation <- main_data$Hormonal.Contraceptives == NA

#subset(main_data )


main_data[which (main_data$Hormonal.Contraceptives == NA), ]

main_data$Hormonal.Contraceptives


#### Subsetting the data to a useful predictor dataset
colnames(main_data)

cancer_predictors <- c("Age", "Number.of.sexual.partners", "First.sexual.intercourse", "Num.of.pregnancies", 
"Smokes", "STDs")

predictor_df <- subset(main_data, select = cancer_predictors)


#### converting "chr" to "int" data type
str(predictor_df)

predictor_df$Number.of.sexual.partners <- as.integer(predictor_df$Number.of.sexual.partners)
predictor_df$First.sexual.intercourse <- as.integer(predictor_df$First.sexual.intercourse)
predictor_df$Num.of.pregnancies <- as.integer(predictor_df$Num.of.pregnancies)
predictor_df$Smokes <- as.integer(predictor_df$Smokes)
#predictor_df$Hormonal.Contraceptives <- as.integer(predictor_df$Hormonal.Contraceptives)
#predictor_df$IUD <- as.integer(predictor_df$IUD)
predictor_df$STDs <- as.integer(predictor_df$STDs)

predictor_df$Birth.control <- as.integer(as.integer(main_data$Hormonal.Contraceptives) | as.integer(main_data$IUD))

str(predictor_df)

#### imputing missing values
sapply(predictor_df, function(x) sum(is.na(x)))

#missing values:
#Age 0
#Number.of.sexual.partners  26
#First.sexual.intercourse 7
#Num.of.pregnancies 56
#Smokes 13
#STDs 105
#Birth.Control 112
####Hormonal.Contraceptives 108
####IUD 117

#imputation:
#Age - none
#Number.of.sexual.partners - 1 (the mode)
#First.sexual.intercourse - 17 (rounded mean)
#Num.of.pregnancies - 1 (mode)
#Smokes - 0 (imbalanced mode)
#STDs - 0 (imbalanced dataset)
#Birth.Control - 1 (imbalanced mode)
####Hormonal.Contraceptives -
####IUD -

predictor_df$Number.of.sexual.partners[is.na(predictor_df$Number.of.sexual.partners)] <- 1
predictor_df$First.sexual.intercourse[is.na(predictor_df$First.sexual.intercourse)] <- round(mean(predictor_df$First.sexual.intercourse, na.rm = TRUE))
predictor_df$Num.of.pregnancies[is.na(predictor_df$Num.of.pregnancies)] <- 1
predictor_df$Smokes[is.na(predictor_df$Smokes)] <- 0
predictor_df$STDs[is.na(predictor_df$STDs)] <- 0
predictor_df$Birth.control[is.na(predictor_df$Birth.control)] <- 1

table(main_data$Dx.Cancer) #18
table(main_data$Dx.CIN) #9
table(main_data$Dx.HPV) #18
table(main_data$Dx) #24
table(main_data$Hinselmann) #35
table(main_data$Schiller) #74
table(main_data$Citology) #44
table(main_data$Biopsy) #55

sum(main_data$Dx.Cancer) / length(main_data$Dx.Cancer) * 100
sum(main_data$Dx.CIN) / length(main_data$Dx.CIN) * 100
sum(main_data$Dx.HPV) / length(main_data$Dx.HPV) * 100
sum(main_data$Dx) / length(main_data$Dx) * 100
sum(main_data$Hinselmann) / length(main_data$Hinselmann) * 100
sum(main_data$Schiller) / length(main_data$Schiller) * 100
sum(main_data$Citology) / length(main_data$Citology) * 100
sum(main_data$Biopsy) / length(main_data$Biopsy) * 100

######## CORRELATION PLOT
corrplot::corrplot(cor(predictor_df))


####### BASIC LOGISTIC REGRESSION MODEL
logReg <-train(x = predictor_df, y = as.factor(main_data$Schiller),
               method = "glm",
               preProcess = c("center","scale"))

#raw coefficients
data.frame(logReg$finalModel$coefficients)

#odds ratios
data.frame(exp(logReg$finalModel$coefficients))

