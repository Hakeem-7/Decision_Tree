#Decision Tree Algo using the C5.0 algorithm by J. Ross Quinlanb (Industry Standard) - Divide and Conquer

curve(-x * log2(x) - (1 - x) * log2(1 - x),
    col = "darkred", xlab = "x", ylab = "Entropy", lwd = 3) #Illustration of entropy; 50-50 split results in maximum entropy

#Identifying risky bank loans using c5.0 decision trees - UCI repository
train = read.csv("a4a_Training.csv")
str(train)
table(train$Label)  #summarizes the data in the feature
ClassVariables = sapply(train, function(x) class(x)) #Class Variables
ClassVariables

Classes <- function(train){
  Class_variables <- sapply(train, function(x) class(x)) #component of the function should be the adjustable parameter
  return(Class_variables)
}
Classes(train_factor)  #rm(cv) - removed the previous function


#train_sample <- sample(1000, 900) - Samples 900 from 1000 observations.
install.packages("C50") #Decision tree algo that implement entropy criterion
?c5.0

library(dplyr)
train_factor <- train %>%
  mutate_at(vars(Label), 
            funs(factor))   #Transforms the integer variable to a factor variable

str(train_factor)

library(C50)
model <- C5.0(train_factor[-1], train_factor$Label) #Decision tree model
model
summary(model)

test = read.csv("a4a_Testing.csv") #the test data is larger than the training data
table(test$Label)
test_factor = test %>%
  mutate_at(vars(Label),
            funs(factor))
Classes(test_factor)      #confirmation test that the mutation worked

# Model performance evaluation
model_pred <- predict(model, test_factor)
#install.packages("gmodels")
library(gmodels)
CrossTable(test_factor$Label, model_pred, prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual', 'predicted')) 

# Model performance improvement - Boosting the accuracy of decision trees
#C5.0 algorithm improved upon the C4.5 algo through the adoption of adaptive boosting.
model_boost <- C5.0(train_factor[-1], train_factor$Label, trials = 10) #Research suggested that "trials = 10" improves tree accuracy by 25%)
model_boost
summary(model_boost)
model_boost_pred <- predict(model_boost, test_factor)
CrossTable(test_factor$Label, model_boost_pred, prop.r = F, prop.c = F, prop.chisq = F,
           dnn = c("actual", "predicted"))

# Addressing false negatives, especially in sensitive cases like bank loans - cost matrix
matrix_dim <- list(c("no", "yes"), c("no", "yes"))
names(matrix_dim) <- c("predicted", "actual")
matrix_dim

#create the confusion matrix
error_cost <- matrix(c(0,1,4,0), nrow = 2,
      dimnames = matrix_dim) #false negative cost the bank 4 times more than false positive
error_cost #No cost assigned for correct predictions in the confusion matrix

# Apply the error cost correction to the original model
model_cost <- C5.0(train_factor[,-1], train_factor$Label, costs = error_cost)

model_cost_pred <- predict(model_cost, test_factor) #Error due to data size


CrossTable(test_factor$Label, model_cost_pred, prop.c = F,
           prop.chisq = F, prop.r = F,
           dnn = c('actual', 'predicted'))



sum(is.na(train_factor)) #This wasn't accounted for in this analysis
