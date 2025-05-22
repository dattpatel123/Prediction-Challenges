getwd()


library(ModelMetrics)
library(rpart)
library(CrossValidation)
library(rpart.plot)

train <- read.csv('UniPredTrain_Students.csv')
uni <- read.csv('UniversityData.csv')

train$original_order <- 1:nrow(train)

train <- merge(train, uni, by='Uni')
train <- train[order(train$original_order), ]
train$original_order <- NULL


head(train)

# Uni 
mod <- rpart(Salary ~ Grad + Tuition+Location+GPA, data = train, control = rpart.control(minsplit = 10, cp = 0.00001,maxdepth = 10), method='anova')


decision <- predict(mod, newdata=train)

mse(train$Salary, decision)


# Test set

test <- read.csv('UniPredTest_Students.csv')
test$original_order <- 1:nrow(test)
test <- merge(test, uni, by='Uni')
test <- test[order(test$original_order), ]

test$original_order <- NULL
decision <- predict(mod, newdata=test)

test$predicted <- decision
head(test)

sub <- read.csv('submission4.csv')
sub$Salary <- decision
head(sub)

write.csv(sub, 'submission.csv', row.names=FALSE)



