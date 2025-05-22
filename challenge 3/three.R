getwd()


library(rpart)
library(CrossValidation)
library(rpart.plot)

train <- read.csv('airbnbTrain.csv')
borough <- read.csv('Boroughs.csv')

train <- merge(train, borough, by.x = "neighbourhood", by.y = "neighborhood")

train <- train[, c('floor',"price",'footage',"avgreview", 'borough', "Deal")]

# New feature
train$price_per_footage <- train$price / train$footage

train
#Split on borough
tree1 <- rpart(Deal ~ ., data = train[train$borough == 'Queens',], method = "class")
tree2 <- rpart(Deal ~ ., data = train[train$borough == 'Brooklyn',], method = "class")
tree3 <- rpart(Deal ~ ., data = train[train$borough == 'Manhattan',], method = "class")


train$model1<-rep('Neutral',nrow(train))
train$model2<-rep('Neutral',nrow(train))
train$model3<-rep('Neutral',nrow(train))


# Predictions for each borough
train[train$borough == 'Queens',]$model1 <- as.character(predict(tree1, newdata = train[train$borough == 'Queens',], type = "class"))
train[train$borough == 'Brooklyn',]$model2 <- as.character(predict(tree2, newdata = train[train$borough == 'Brooklyn',], type = "class"))
train[train$borough == 'Manhattan',]$model3 <- as.character(predict(tree3, newdata = train[train$borough == 'Manhattan',], type = "class"))




# Combined model
tree_combined<-rpart(Deal~model1+model2+model3, data=train, method='class')


# Cross validate
cross_validate(train,tree_combined,10,0.7)
train$predicted <- predict(tree_combined, newdata = train,type='class')
mean(train$predicted == train$Deal)





# Applying to test set
test <- read.csv('airbnbTestStudents.csv')
sub <- read.csv('submission_airbnb.csv')
test <- merge(test, borough, by.x = "neighbourhood", by.y = "neighborhood")


test
test$price_per_footage <- test$price / test$footage


test$model1<-rep('Neutral',nrow(test))
test$model2<-rep('Neutral',nrow(test))
test$model3<-rep('Neutral',nrow(test))

# Prediction with models
test[test$borough == 'Queens',]$model1 <- as.character(predict(tree1, newdata = test[test$borough == 'Queens',], type = "class"))
test[test$borough == 'Brooklyn',]$model2 <- as.character(predict(tree2, newdata = test[test$borough == 'Brooklyn',], type = "class"))
test[test$borough == 'Manhattan',]$model3 <- as.character(predict(tree3, newdata = test[test$borough == 'Manhattan',], type = "class"))


# final prediction using our combined model
test$predicted <- predict(tree_combined, newdata = test,type='class')
head(test)

sub <- merge(sub, test[, c("id", "predicted")], by.x = "ID", by.y = "id")
sub$Deal <- sub$predicted
sub <- sub[,c("ID", "Deal")]

head(sub)
table(train$Deal)
table(sub$Deal)

write.csv(sub, 'submission3.csv', row.names=FALSE)


