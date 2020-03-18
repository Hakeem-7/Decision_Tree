#Decision Tree Algo using the C5.0 algorithm by J. Ross Quinlanb (Industry Standard)

curve(-x * log2(x) - (1 - x) * log2(1 - x),
    col = "darkred", xlab = "x", ylab = "Entropy", lwd = 3) #Illustration of entropy; 50-50 split results in maximum entropy

#Identifying risky bank loans using c5.0 decision trees - UCI repository
train = read.csv("a4a_Training.csv")
str(train)
table(train$Label)  #summarizes the data in the feature
ClassVariables = sapply(train, function(x) class(x)) #Class Variables
ClassVariables

#train_sample <- sample(1000, 900) - Samples 900 from 1000 observations.
install.packages("C50") #Decision tree algo that implement entropy criterion
?c5.0

library(dplyr)

train_factor <- train %>%
  mutate_at(vars(Label), 
            funs(factor))

str(train_factor)


library(C50)
model <- C5.0(train_factor[-1], train_factor$Label) #Decision tree model






  

