## project ##
library(e1071)
library(neuralnet)
library(caret)
library(forecast)
library(rpart) 
library(rpart.plot) ### rpart and rpart.plot libraries are for the tree analysis
library(randomForest)
library(fastDummies)
library(FNN)
#0: Reading in data and preprocess
setwd("C:/Users/jj_bu/Desktop/TTU/Classes/Spring/Machine Learning/Project")
movie_data <- read.csv("Movies.csv")
View(movie_data)


#Setting a seed for consistent train/valid split 
set.seed(4321)

#Creating dummy variables
movie_data$X3D_Available=as.factor(movie_data$X3D_Available)
movie_data$Genre=as.factor(movie_data$Genre)
movie_data=dummy_cols(.data = movie_data,select_columns =c("X3D_Available","Genre"),remove_first_dummy = TRUE)

### Partition data first
train_index <- sample(rownames(movie_data), 0.6*dim(movie_data)[1])  
valid_index <- setdiff(rownames(movie_data), train.index)  
train_data<-movie_data[train_index, ]
valid_data<-movie_data[valid_index, ]


varlist<-c(1,2,3,4,5,6,7,8,10,12,13)
###Creating normilization for everything that would need it
train_index<-sample(rownames(movie_data), dim(movie_data)[1]*0.6)
valid_index<-setdiff(rownames(movie_data),train_index)
norm.values <- preProcess(movie_data[,varlist], method="range")
train.norm.df <- predict(norm.values, train_data)
valid.norm.df <- predict(norm.values, valid_data)

#Checking Correlations
round(cor(movie_data[,c(1:8,10,12:17)]),2)


#Trailer_Views ~ Marketing_Expense+Production_Expense+Budget+
#Movie_Length+Lead_.Actor_Rating+Genre_Comedy+Genre_Drama+Genre_Thriller



### 1: Tree ###
mytree<- rpart(Trailer_Views ~ Marketing_Expense+
                 Production_Expense+Budget+Movie_Length+Lead_.Actor_Rating+Genre_Comedy+
                 Genre_Drama+Genre_Thriller, data = train.norm.df, method = "anova")

prp(mytree)  ## plot the tree

### Predict using the validation data
predicted_values <- predict(mytree, newdata=valid.norm.df)

### Prediction performance
accuracy(predicted_values, valid.norm.df$Trailer_Views)


### Random forest

### Train the model using the training data
myforest <- randomForest(Trailer_Views ~ Marketing_Expense+
                           Production_Expense+Budget+Movie_Length+Lead_.Actor_Rating+Genre_Comedy+
                           Genre_Drama+Genre_Thriller, data = train.norm.df) 

### Predict using the validation data
predicted_values_forest <- predict(myforest, newdata=valid.norm.df)

### Prediction performance 
accuracy(predicted_values_forest, valid.norm.df$Trailer_Views)





### 2: Linear Regression ###

### Run linear regression model on the training data
mymodel<-lm(Trailer_Views ~ Marketing_Expense+
              Production_Expense+Budget+Movie_Length+Lead_.Actor_Rating+Genre_Comedy+
              Genre_Drama+Genre_Thriller, data=train.norm.df)
summary(mymodel)  ## regression results

### Predict using the validation data
predicted_views<- predict(mymodel, newdata=valid.norm.df)

### Prediction errors
accuracy(predicted_views,valid.norm.df$Trailer_Views)




### 3: ANN ###

# 1 hidden layer with 2 nodes
nn1 <- neuralnet(Trailer_Views ~ Marketing_Expense+
                   Production_Expense+Budget+Movie_Length+Lead_.Actor_Rating+Genre_Comedy+
                   Genre_Drama+Genre_Thriller, data = train.norm.df, linear.output = TRUE, hidden = 2)

plot(nn1)

prediction1<-compute(nn1, valid.norm.df)

# RMSE and other measures
accuracy(prediction1$net.result[,1], valid.norm.df$Trailer_Views)

# 1 hidden layer with 5 nodes
nn2 <- neuralnet(Trailer_Views ~ Marketing_Expense+
                   Production_Expense+Budget+Movie_Length+Lead_.Actor_Rating+Genre_Comedy+
                   Genre_Drama+Genre_Thriller, data = train.norm.df, linear.output = TRUE, hidden =5)

plot(nn2)

prediction2<-compute(nn2, valid.norm.df)

# RMSE and other measures
accuracy(prediction2$net.result[,1], valid.norm.df$Trailer_Views)

# 2 hidden layers with 2 nodes in each
nn3 <- neuralnet(Trailer_Views ~ Marketing_Expense+
                   Production_Expense+Budget+Movie_Length+
                   Lead_.Actor_Rating+Genre_Comedy+
                   Genre_Drama+Genre_Thriller, 
                   data = train.norm.df, linear.output = TRUE, hidden = c(2,2))

plot(nn3)

prediction3<-compute(nn3, valid.norm.df)

# RMSE and other measures
accuracy(prediction3$net.result[,1], valid.norm.df$Trailer_Views)



### 4: SVM ###
svm1 <- svm(Trailer_Views ~ Marketing_Expense+
              Production_Expense+Budget+Movie_Length+Lead_.Actor_Rating+Genre_Comedy+
              Genre_Drama+Genre_Thriller, data = train.norm.df)


prediction1<-predict(svm1, valid.norm.df)

# RMSE and other measures
accuracy(prediction1, valid.norm.df$Trailer_Views)

### 5: KNN ###

accuracy.df <- data.frame(k = seq(1, 45, 1), RMSE = rep(0, 45))

# compute kNN for different k on validation
for(i in 1:45) {
  knn.pred <- knn.reg(train.norm.df[,c(1,2,3,4,5,6,7,8,10,12)], valid.norm.df[,c(1,2,3,4,5,6,7,8,10,12)], 
                      y = train.norm.df[, 13], k = i)
  accuracy.df[i, 2] <- accuracy(knn.pred$pred, valid.norm.df[, 13])[2] # [2] at the end is saying that we look at the 2nd error metric in the accuracy function output, which is RMSE 
}
accuracy.df$RMSE
min(accuracy.df$RMSE)
#Best is k=13

knn.pred.final<- knn.reg(train.norm.df[,c(1,2,3,4,5,6,7,8,10,12)], valid.norm.df[,c(1,2,3,4,5,6,7,8,10,12)], 
                         y = train.norm.df[, 13], k = 13)

accuracy(knn.pred.final$pred,valid.norm.df$Trailer_Views)




