#Linear Regression

#Append Response variable to the dataset
final_data <- data.frame(c(reduced_pca_data, mydata[c("Response")]))
final_data$Response<-as.numeric(final_data$Response)

#Divinding our dataset into train(80%) and test(20%) 
set.seed(123)
index = sample(seq_len(nrow(Filtered_Data_new)), size = floor(0.8*nrow(Filtered_Data_new)))
train_data <- final_data[index, ]
test_data <- final_data[-index, ]


#Applying Linear Regression on train data
linearregression_model <-lm(Response ~., data =train)

summary(linearregression_model)

#Residual standard error: 1.981 on 49265 degrees of freedom
#Multiple R-squared:  0.3521,	Adjusted R-squared:  0.3506 
#F-statistic: 230.9 on 116 and 49265 DF,  p-value: < 2.2e-16

#Predicting Response Variable
prediction <- predict(linearregression_model, test)

#Calculate accuracy
accuracy(prediction,test$Response)

#ME    RMSE      MAE       MPE     MAPE
#Test set 0.007678344 1.96932 1.527038 -39.83762 62.52931
