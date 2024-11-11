install.packages("party")
install.packages("tree")
install.packages("rpart")
install.packages("randomForest")
library(party)
library(tree)
library(rpart)
library(randomForest)


# Question 1
cw.all = read.csv('creditworthiness.csv')

cw.known = subset(cw.all, credit.rating > 0 )
cw.unknown = subset(cw.all, credit.rating == 0 )

cw.known$credit.rating = as.factor(cw.known$credit.rating)


cw.train <- cw.known[1:(nrow(cw.known)/2),]
dim(cw.train)
cw.test <- cw.known[-(1:(nrow(cw.known)/2)),]
dim(cw.test)


medianCust = data.frame()
thedata = c(0,1,1,0,3,0,3,3,0,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3, 3,3,3,3,3,3,3,3,3,3,3,3)

medianCust = rbind(medianCust, thedata)

colnames(medianCust) = names(cw.all)[-46]


# Question 2
tree.cw.train= tree(credit.rating~., data=cw.train)
tree.cw.train

#b) 
tree.pred = predict(tree.cw.train, cw.test[,-46], type = "class")
tree.pred
# predict using "medianCust"
cust.pred = predict(tree.cw.train, medianCust, type = "class")
cust.pred

#c)
confusion = with(cw.test, table(tree.pred, credit.rating))
confusion

#c)
sum(diag(confusion)/sum(confusion))