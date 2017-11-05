install.packages("readr")
install.packages("xgboost")

library(readr)
library(xgboost)
library(forecast)

a <- data.frame(c(final_data, mydata[c("Response")]))
a$Response<-as.numeric(a$Response)

#Divinding our dataset into train(80%) and test(20%) 
set.seed(123)
index = sample(seq_len(nrow(Filtered_Data_new)), size = floor(0.8*nrow(Filtered_Data_new)))
train_data <- final_data[index, ]
test_data <- final_data[-index, ]


#removing the ID column and Response column from the dataframe- in train
#

Colum_names <- names(train)[2:(ncol(train)-1)]


#Through manual observation, its being observed that all days, 
#assuming text variables are categorical & replacing them with numeric values in 
#both the train and test dataframe

for (i in Colum_names) {
  if (class(train[[i]])=="character") {
    levels <- unique(c(train[[i]], test[[i]]))
    train[[i]] <- as.integer(factor(train[[i]], levels=levels))
  }
}

#Converting Categorized features to numeric

for (i in Colum_names) {
  if (class(train[[i]])=="character") {
    levels <- unique(c(train[[i]], test[[i]]))
    test[[i]]  <- as.integer(factor(test[[i]],  levels=levels))
    
  }
}


#Starting XGBoost for the train data

XgData <- xgboost(data = data.matrix(train[,Colum_names]),
                  label = train$Response,
                  eta = 0.5,
                  depth=15,
                  nrounds= 15,
                  objective = "reg:linear",
                  eval_metric = "rmse")


#Making the predictions through 
#PredictData <- data.frame(Id=test$Id)

Response2 <- as.integer(round(predict(XgData, data.matrix(test[,Colum_names]))))


##accuracy(PredictData$Response2,test$Response)
###############ME     RMSE      MAE       MPE     MAPE
#Test set 0.01660166 1.892765 1.353535 -36.39322 57.41654
#The function accuracy gives you multiple measures of accuracy of the model fit:
#mean error (ME), root mean squared error (RMSE), mean absolute error (MAE), 
#mean percentage error (MPE), mean absolute percentage error (MAPE),
#mean absolute scaled error (MASE) and the first-order autocorrelation coefficient (ACF1).
#It is up to you to decide, based on the accuracy measures, whether you consider this a
#good fit or not. For example, mean percentage error of nearly -70% does not look good to
#me in general, but that may depend on what your series are and how much predictability you 
#may realistically expect.

accuracy(Response2,test$Response)


#Plotting the model
model <- xgb.dump(XgData, with.stats = T)
model[1:10] #This statement prints top 10 nodes of the model
# Get the feature real names
names <- dimnames(data.matrix(train[,-1]))[[2]]
# Compute feature importance matrix
importance_matrix <- xgb.importance(names, model = XgData)
# Nice graph
xgb.plot.importance(importance_matrix[1:10,])

#cc<-importance_matrix[,2]
#In case last step does not work for you because of a version issue, you can try following :
#barplot(cc)

#Testing our model
test<- chisq.test(test$Response,Response2)
print(test)
