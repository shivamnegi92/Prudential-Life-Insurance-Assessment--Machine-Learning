# Decision Tree

final_data <- data.frame(c(final_data, mydata[c("Response")]))
final_data$Response<-as.numeric(final_data$Response)

#Divide the given dataset
train<-final_data[1:49382,]
test<-final_data[49383:59382,]

#Generate the model for Decision tree
mytree <- rpart(Response ~ .,data = train, method = "anova", control=rpart.control(cp=0.01))

# detailed summary of splits
summary(mytree) 

# display the results 
printcp(mytree) 

# visualize cross-validation results
plotcp(mytree) 
prp(mytree)

#Predict the Response Variable
myprediction <- predict(mytree, test)
View(round(myprediction))

#Calculate accuracy of the model
accuracy(myprediction,test$Response)

