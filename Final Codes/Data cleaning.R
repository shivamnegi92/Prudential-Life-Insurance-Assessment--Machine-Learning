#installing all packages 
install.packages("forecast")
install.packages("ade4")
install.packages("forecastHybrid")
install.packages("e1071")

library(forecast)
library(ade4)
library(e1071)
library(ggplot2)
library("rpart")
library("rpart.plot")


# Loading our dataset into R
mydata <- dataset

#1. Pre-processing and cleaning the data
#REmoving columns which are having more than 60% data value as null

dataclean1<-mydata[, -which(colMeans(is.na(mydata)) > .6)]
mydata$Id <- NULL

#Verifying the columns which are removed
removedcoumns<-mydata[,which(colMeans(is.na(mydata)) > 0.6)]

#Family_Hist_5 Medical_History_10 Medical_History_15 Medical_History_24 Medical_History_32 
#38                 48                 53                 62                 70                   53                  62                  70


#Setting missing values to mean value 
#Finding columns having missing values
missingvalues <- c(unlist(lapply(dataclean1, function(x) any(is.na(x)))))

#Finding columns having missing values
View(missingvalues)

#Employment_Info_1   Employment_Info_4   Employment_Info_6 Insurance_History_5       Family_Hist_2 
#TRUE                TRUE                TRUE                TRUE                TRUE 
#Family_Hist_3       Family_Hist_4   Medical_History_1 
#TRUE                TRUE                TRUE

#Filling missing values with mean values for the obtained columns
dataclean1$Employment_Info_1[is.na(dataclean1$Employment_Info_1)] <- mean(dataclean1$Employment_Info_1, na.rm = T)
dataclean1$Employment_Info_4[is.na(dataclean1$Employment_Info_4)] <- mean(dataclean1$Employment_Info_4, na.rm = T)
dataclean1$Employment_Info_6[is.na(dataclean1$Employment_Info_6)] <- mean(dataclean1$Employment_Info_6, na.rm = T)
dataclean1$Insurance_History_5[is.na(dataclean1$Insurance_History_5)] <- mean(dataclean1$Insurance_History_5, na.rm = T)
dataclean1$Family_Hist_2[is.na(dataclean1$Family_Hist_2)] <- mean(dataclean1$Family_Hist_2, na.rm = T)
dataclean1$Family_Hist_4[is.na(dataclean1$Family_Hist_4)] <- mean(dataclean1$Family_Hist_4, na.rm = T)
dataclean1$Medical_History_1[is.na(dataclean1$Medical_History_1)] <- mean(dataclean1$Medical_History_1, na.rm = T)
dataclean1$Family_Hist_3[is.na(dataclean1$Family_Hist_3)] <- mean(dataclean1$Family_Hist_3, na.rm = T)


#Encoding Categorical variables into numberical variables using 1 to C Coding
Categorical_data <- mydata[,c("Medical_History_1","Product_Info_1", "Product_Info_2", "Product_Info_3", "Product_Info_5", "Product_Info_6", "Product_Info_7", "Employment_Info_2", "Employment_Info_3", "Employment_Info_5", "InsuredInfo_1", "InsuredInfo_2", "InsuredInfo_3", "InsuredInfo_4", "InsuredInfo_5", "InsuredInfo_6", "InsuredInfo_7", "Insurance_History_1", "Insurance_History_2", "Insurance_History_3", "Insurance_History_4", "Insurance_History_7", "Insurance_History_8", "Insurance_History_9", "Family_Hist_1", "Medical_History_2", "Medical_History_3", "Medical_History_4", "Medical_History_5", "Medical_History_6", "Medical_History_7", "Medical_History_8", "Medical_History_9", "Medical_History_11", "Medical_History_12", "Medical_History_13", "Medical_History_14", "Medical_History_16", "Medical_History_17", "Medical_History_18", "Medical_History_19", "Medical_History_20", "Medical_History_21", "Medical_History_22", "Medical_History_23", "Medical_History_25", "Medical_History_26", "Medical_History_27", "Medical_History_28", "Medical_History_29", "Medical_History_30", "Medical_History_31", "Medical_History_33", "Medical_History_34", "Medical_History_35", "Medical_History_36", "Medical_History_37", "Medical_History_38", "Medical_History_39", "Medical_History_40", "Medical_History_41")]
converted_data <- acm.disjonctif(Categorical_data)


#Combinging for continuous data 
Continuous_data <- mydata[c("Product_Info_4", "Ins_Age", "Ht", "Wt", "BMI", "Employment_Info_1", "Employment_Info_4", "Employment_Info_6", "Insurance_History_5", "Family_Hist_2", "Family_Hist_4")]

#Combinging for dummy data
data_dummy<-mydata[c("Medical_Keyword_1","Medical_Keyword_2","Medical_Keyword_3","Medical_Keyword_4","Medical_Keyword_5","Medical_Keyword_6","Medical_Keyword_7","Medical_Keyword_8","Medical_Keyword_9","Medical_Keyword_10","Medical_Keyword_11","Medical_Keyword_12","Medical_Keyword_13","Medical_Keyword_14","Medical_Keyword_15","Medical_Keyword_16","Medical_Keyword_17","Medical_Keyword_18","Medical_Keyword_19", "Medical_Keyword_20", "Medical_Keyword_21", "Medical_Keyword_22", "Medical_Keyword_23","Medical_Keyword_24", "Medical_Keyword_25", "Medical_Keyword_26", "Medical_Keyword_27", "Medical_Keyword_28", "Medical_Keyword_29","Medical_Keyword_30", "Medical_Keyword_31", "Medical_Keyword_32", "Medical_Keyword_33","Medical_Keyword_34", "Medical_Keyword_35","Medical_Keyword_36", "Medical_Keyword_37", "Medical_Keyword_38", "Medical_Keyword_39", "Medical_Keyword_40", "Medical_Keyword_41", "Medical_Keyword_42", "Medical_Keyword_43", "Medical_Keyword_44", "Medical_Keyword_45","Medical_Keyword_46", "Medical_Keyword_47","Medical_Keyword_48")]

#Merging the categorical, continuous and dummy data into final dataset 
Merged_data <- data.frame(c(converted_data, data_cntg,data_dummy))

#Performing Dimensionality Reduction
#Calculating PCA on data
pcadata <- prcomp(merged_data)
summary(pcadata)
plot(final_pca_data)

#After plotting graph and manual observation of PCA Components,we chose 116 PCA Components having 90% Variance
reduced_pca_data <- data.frame(pcadata$x[,1:116])


#Append Response variable to the dataset
final_data <- data.frame(c(reduced_pca_data, mydata[c("Response")]))
final_data$Response<-as.numeric(final_data$Response)

#Divinding our dataset into train(80%) and test(20%) 
set.seed(123)
index = sample(seq_len(nrow(Filtered_Data_new)), size = floor(0.8*nrow(Filtered_Data_new)))
train_data <- final_data[index, ]
test_data <- final_data[-index, ]