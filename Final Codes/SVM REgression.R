#SVM Regression

#Calculating PCA for final consolidated data.
pca_data <- prcomp(final_data)

#Examining the data for summary.
#summary(pca_data)

#plot(pca_data)
#pca_data

#After manual observing PCA Components, We decided to take first 75 PCA Components showing 90% Cumulative proportion of Variance.
Filtered_data <- data.frame(final_pca_data$x[,1:75])
#Concatenated the final data frame with response column.
final_data <- data.frame(c(Filtered_data, mydata[c("Response")]))
final_data$Response<-as.numeric(final_data$Response)

#Dividing the data into train and test
train<-final_data[1:5000,]
test<-final_data[4000:4500,]

#Tuning SVM for finding best parameters for all possible 4 kernels.
obj<-tune(svm, Response~.,kernel ="radial", data= train, ranges=list(gamma=10^(-2:2), cost=10^(-2:2)))
summary(obj)

obj1<-tune(svm, Response~.,kernel ="linear", data= train, ranges=list(cost=10^(-2:2)))
summary(obj1)

obj2<-tune(svm, Response~.,kernel ="sigmoid", data= train, ranges=list(gamma=10^(-2:2), cost=10^(-2:2)))
summary(obj2)

obj3<-tune(svm, Response~.,kernel ="polynomial", data= train, ranges=list(gamma=10^(-2:2), cost=10^(-2:2)))
summary(obj3)

#Applying SVM with the best parameters we found after tuning. 
svm_model = svm(Response ~ ., kernel = "radial", cost =1 ,gamma=0.01, data = train, scale = F)

#Predicting the Values using model created using SVM.
predictions <-  svm_model(model1, test[-c(Response)])

#Validating accuracy of predicted results.
accuracy(predictions,test$Response)

