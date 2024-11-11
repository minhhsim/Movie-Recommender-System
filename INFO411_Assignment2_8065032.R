library(arules)
library(arulesViz)
library(party)
library(tree)
library(rpart)
library(rpart.plot)
library(randomForest)
library(MASS)
library(rpart)
library(e1071)
library(gtsummary)
library(tree)
library(caret)


#Question 1
# Read the data set
cw.all <- read.csv("creditworthiness.csv")
# look at the records
print(cw.all)

# Separate the records with no credit rating, that is, credit rating equals 0 from those with credit rating
cw.known <- subset(cw.all,credit.rating > 0)
cw.unknown <- subset(cw.all,credit.rating == 0)

# Split the records with known credit rating into training and testing datasets
cw.train <- cw.known[1:(nrow(cw.known)/2),] # first half
cw.test <- cw.known[-(1:(nrow(cw.known)/2)),] # second half

# Create a data frame for the hypothetical customer
median_cust = data.frame()
newdata = c(0,1,1,0,3,0,3,3,0,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3)
median_cust = rbind(median_cust, newdata)
colnames(median_cust) = names(cw.known)[-46]



#Question 2

# Report the resulting tree.
tree.cw.train= tree(as.factor(credit.rating)~., data=cw.train)
tree.cw.train

#Predict the credit rating of the hypothetical "median" customer
tree.pred = predict(tree.cw.train, cw.test[,-46],type="class")
tree.pred

cust.pred = predict(tree.cw.train, median_cust, type="class")
cust.pred

#Produce the confusion matrix for predicting the credit rating from the tree produce earlier
confusion = with(cw.test, table(tree.pred, credit.rating))
confusion

#confusion = confusionMatrix(cw.test,table(cust.pred, credit.rating))
#overall.accuracy <- confusion$overall['Accuracy']

# calculate the accuracy 
# divide the sum of the diagonal values (top left down to bottom right) by the sum of all the values in the matrix
sum(diag(confusion))/sum(confusion)

# Compute the entropy before the split
# get the count of all classes in credit.rating using the table() function
beforeCountFreq = table(cw.train$credit.rating)
#find the probability of each class
beforeClassProb = beforeCountFreq/sum(beforeCountFreq)
#calculate entropy (before split)
beforeEntropy = -sum(beforeClassProb * log2(beforeClassProb))

# Compute the entropy for 'functionary' feature value 0
# functionary == 0
countFreq0 = table(cw.train$credit.rating[cw.train$functionary == 0])
classProb0 = countFreq0/sum(countFreq0)
(functionaryEnt0 = -sum(classProb0 * log2(classProb0)))

# Compute the entropy for 'functionary' feature value 1
# functionary == 1
countFreq1 = table(cw.train$credit.rating[cw.train$functionary == 1])
classProb1 = countFreq1/sum(countFreq1)
(functionaryEnt1 = -sum(classProb1 * log2(classProb1)))

# compute the numerical value of the gain in entropy
ent = (beforeEntropy - (functionaryEnt0 * sum(countFreq0) +
                        functionaryEnt1 * sum(countFreq1)) /
                        sum(sum(countFreq0) + sum(countFreq1)))

# Fit a random forest model to the training set to try to improve the prediction
rf.cw.train = randomForest(as.factor(credit.rating)~., data = cw.train)
rf.cw.train
rf.pred = predict(rf.cw.train, cw.test[,-46])

# produce the confusion matrix between the test dataset and predicted value
confusionRF = with(cw.test, table(rf.pred, credit.rating))
sum(diag(confusionRF))/sum(confusionRF)

# improve the random forest model
# Fit to a model using randomForest after the tuning
RFTuned.cw.train = randomForest(as.factor(credit.rating)~., data = cw.train, mtry
                                = 12, ntree=500, stepFactor=2, improve=0.2)
RFTuned.pred = predict(RFTuned.cw.train, cw.test[,-46])

# Produce confusion matrix after the tuning
confusionRFTuned = with(cw.test, table(RFTuned.pred, credit.rating))

# Calculate the accuracy rate after the tuning
sum(diag(confusionRFTuned))/sum(confusionRFTuned)


# Question 3
# Using default settings for SVM() from the e1071 package, fir a support
# vector machine to predict the credit ratings of customer using all of
# the other variables in the dataset
svmfit = svm(as.factor(credit.rating) ~ ., data = cw.train, kernel = "radial")
print(svmfit)

# Predict the credit rating of a hypothetical "median" customer,
# that is, one with the attributes listed in Table 1 (in the last page)
# of the assignment specification
predict(svmfit, median_cust, decision.values = TRUE)

# Predict the confusion matrix for predicting the credit rating from the
# SVM on the test set
svm.pred = predict(svmfit, cw.test[,-46])

# Generate the confusion matrix
confusionSVM = with(cw.test, table(svm.pred, credit.rating))
confusionSVM

# Overall accuracy rate
sum(diag(confusionSVM))/sum(confusionSVM)

#improve the SVM model, either automatically or manually tune the SVM model
summary(tune.svm(as.factor(credit.rating) ~ ., data = cw.train,
                 kernel = "radial",cost = 10^c(0:2), gamma = 10^c(-4:-1)))
# Fit a model using SVM
svmTuned = svm(as.factor(credit.rating) ~ ., data = cw.train, kernel = "radial",
               cost=100,
               gamma = 0.0001)
# Predict the values on test set
svmTuned.pred = predict(svmTuned, cw.test[,-46])

# Produce confusion matrix
confusionTunedSVM = with(cw.test, table(svmTuned.pred, credit.rating))

# Overall accuracy rate
sum(diag(confusionTunedSVM))/sum(confusionTunedSVM)


#Question 4

nb = naiveBayes(as.factor(credit.rating)~. ,data=cw.train)
predict(nb, median_cust, type='class')
predict(nb, median_cust, type='raw')
#predict the values on test set
nb.pred = predict(nb, cw.test[,-46])

#produce confusion matrix
confusionNB = with(cw.test, table(nb.pred, credit.rating))
confusionNB

# Reproduce the first 20 or so lines of the R output for the Naive Bayes
# fit, and use them to explain the steps involved in making this prediction.
nb

#calculate the accuracy rate
sum(diag(confusionNB))/sum(confusionNB)



# Question 6

# Use logistic regression model to predict whether a customer gets a
# credit rating of A using all of the other variables in the dataset, with no interactions
glm.fit <- glm(as.factor((credit.rating==1))~., data = cw.train, family = binomial)
glm.fit

# print the summary of the glm model 
options(width = 130)
summary(glm.fit)

# Use SVM model to predict the training set
# Fit an SVM model of your choice to the training set
summary(tune.svm(as.factor((credit.rating==1)) ~ ., data = cw.train,
                 kernel = "radial",cost = 10^c(-2:2), gamma = 10^c(-4:1),
                 type="C"))
(svm2 = svm(I(credit.rating == 1)~ ., data = cw.train, type = "C"))

# Predict the values on test set[SVM]
svm.fit.pred = predict(svm2, cw.test[,-46], decision.values =TRUE)

# Predict the values on test set[GLM]
glm.fit.pred = predict(glm.fit, cw.test[,-46])

library(ROCR)
# Make prediction using SVM
confusionSVM = prediction(-attr(svm.fit.pred, "decision.values"),
                          cw.test$credit.rating == 1)

# Create rocs curve based on prediction
rocsSVM <- performance(confusionSVM, "tpr", "fpr")

#make prediction using Logidtic Regression
confusionGLM = prediction(glm.fit.pred, cw.test$credit.rating == 1)

#create rocs curve based on prediction
rocsGLM <- performance(confusionGLM, "tpr", "fpr")

# Produce ROC chart
# Plot the graph
plot(rocsGLM, col=1)
plot(rocsSVM, col= 2 ,add=TRUE)
abline(0, 1, lty = 3)
# Add the legend to the graph
legend(0.6, 0.6, c('glm','svm'), 1:2)

