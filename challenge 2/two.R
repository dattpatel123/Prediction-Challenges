getwd()


colnames(train)
head(train)
library(rpart)
install.packages("devtools")  
devtools::install_github("devanshagr/CrossValidation") 
library(CrossValidation)
library(rpart.plot)

# Combining Models better Code

train <- read.csv('couplesTrain.csv')
train$IncomeRatio <- train$GroomInc / train$BrideInc
train$IncomeDifference <- (train$GroomInc-train$BrideInc)
# Create a new column "Same_MB", PRETTY GOOD
train$Same_MB <- "False"
# Assign "Same" to rows where brideMB equals groomMB
train$Same_MB[train$BrideMB == train$GroomMB] <- "True"

train$model1 <- rep('Failure',nrow(train))
train$model2 <- rep('Failure',nrow(train))


# Apply model 1
tree1 <- rpart(Outcome~., data = train ,method = "class")
train$model1<- predict(tree1, train, type="class")

# Apply model 2
decision <- rep('Failure',nrow(train))

decision[train$GroomMB == train$BrideMB] <- 'Failure'

train$model2 <-decision

#Combine both models, about 92% consistent
tree_combined<-rpart(Outcome~., data=train, method='class')
round(mean(train$Outcome == predict(tree_combined, train, type='class')),5)
#rpart.plot(tree_combined)
cross_validate(train,tree_combined,5,0.7)
table(train$Outcome)








# Applying on test set
test <- read.csv('CouplesTestStudents.csv')

test$IncomeRatio <- test$GroomInc / test$BrideInc
test$IncomeDifference <- (test$GroomInc-test$BrideInc)

test$Same_MB <- "False"
# Assign "Same" to rows where brideMB equals groomMB
test$Same_MB[test$BrideMB == test$GroomMB] <- "True"


test$model1 <- rep('Failure',nrow(test))
test$model2 <- rep('Failure',nrow(test))


# Apply model 1

test$model1<- predict(tree1, test, type="class")

# Apply model 2
decision <- rep('Failure',nrow(test))

decision[test$GroomMB == test$BrideMB] <- 'Failure'

test$model2 <-decision

#Combine both models, about 92% consistent

decision <- predict(tree_combined, newdata= test, type='class')

table(decision)


sub<-read.csv('https://raw.githubusercontent.com/paramshah4/data101_tutorial/main/files/dataset/submissionC.csv')
sub$Outcome <- decision
head(sub)
table(sub$Outcome)

write.csv(sub, 'submission.csv', row.names=FALSE)



