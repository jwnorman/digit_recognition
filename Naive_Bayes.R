# Load data, libraries
print(load('~/digitsTrain.rda'))
print(load('~/digitsTest.rda'))
training <- sampleTrain
testing <- newTest
rm(sampleTrain, newTest)
library("e1071")
library("MASS")
library("nortest")
library("moments")
library("MVN")
library("caret")

# Principal components
pc <- princomp(training[,-1])

# Code to produce /Naive_Bayes/cm.Rda
{
# train:	28000
# pcs:		59
data <- training[,-1]
labels <- as.factor(training[,1])
testlabels <- as.factor(testing[,1])
reduced <- pc$scores[,1:59]
test <- as.matrix(testing[,-1]) %*% pc$loadings[,1:59]
model <- naiveBayes(reduced, labels)
predtest <- predict(model, test) # 3 min
cm_bayes <- confusionMatrix(predtest, testlabels)
1 - cm_bayes$byClass[,"Sensitivity"]
}

# Naive Bayes with blocking with 5-fold cv
# End up not using blocking because it performs very badly on training data.

# Find split indices for doing CV
indices <- nrow(training)/5
start <- proc.time()
# 122 seconds
model14_1 <- naiveBayes(fourteen, labels, subset = setdiff(1:nrow(training), 1*(1:indices)))
pred14_1 <- predict(model14_1, fourteen[1*(1:indices),])
cm14_1 <- confusionMatrix(pred14_1, labels[1*(1:indices)]) # .4987

model14_2 <- naiveBayes(fourteen, labels, subset = setdiff(1:nrow(training), 2*(1:indices)))
pred14_2 <- predict(model14_2, fourteen[2*(1:indices),])
cm14_2 <- confusionMatrix(pred14_2, labels[2*(1:indices)]) # .4964

model14_3 <- naiveBayes(fourteen, labels, subset = setdiff(1:nrow(training), 3*(1:indices)))
pred14_3 <- predict(model14_3, fourteen[3*(1:indices),])
cm14_3 <- confusionMatrix(pred14_3, labels[3*(1:indices)]) # .5005

model14_4 <- naiveBayes(fourteen, labels, subset = setdiff(1:nrow(training), 4*(1:indices)))
pred14_4 <- predict(model14_4, fourteen[4*(1:indices),])
cm14_4 <- confusionMatrix(pred14_4, labels[4*(1:indices)]) # .4918

model14_5 <- naiveBayes(fourteen, labels, subset = setdiff(1:nrow(training), 5*(1:indices)))
pred14_5 <- predict(model14_5, fourteen[5*(1:indices),])
cm14_5 <- confusionMatrix(pred14_5, labels[5*(1:indices)]) # .507

avgaccuracy14 <- mean(c(cm14_1$overall[1], cm14_2$overall[1], cm14_3$overall[1], cm14_4$overall[1], cm14_5$overall[1]))
########################################
# 7 x 7 # 32 * 5 = 160 seconds = 2min40sec
########################################
# 32 seconds
model7_1 <- naiveBayes(seven, labels, subset = setdiff(1:nrow(training), 1*(1:indices)))
pred7_1 <- predict(model7_1, seven[1*(1:indices),])
cm7_1 <- confusionMatrix(pred7_1, labels[1*(1:indices)]) # .4987

model7_2 <- naiveBayes(seven, labels, subset = setdiff(1:nrow(training), 2*(1:indices)))
pred7_2 <- predict(model7_2, seven[2*(1:indices),])
cm7_2 <- confusionMatrix(pred7_2, labels[2*(1:indices)])

model7_3 <- naiveBayes(seven, labels, subset = setdiff(1:nrow(training), 3*(1:indices)))
pred7_3 <- predict(model7_3, seven[3*(1:indices),])
cm7_3 <- confusionMatrix(pred7_3, labels[3*(1:indices)])

model7_4 <- naiveBayes(seven, labels, subset = setdiff(1:nrow(training), 4*(1:indices)))
pred7_4 <- predict(model7_4, seven[4*(1:indices),])
cm7_4 <- confusionMatrix(pred7_4, labels[4*(1:indices)])

model7_5 <- naiveBayes(seven, labels, subset = setdiff(1:nrow(training), 5*(1:indices)))
pred7_5 <- predict(model7_5, seven[5*(1:indices),])
cm7_5 <- confusionMatrix(pred7_5, labels[5*(1:indices)])

avgaccuracy7 <- mean(c(cm7_1$overall[1], cm7_2$overall[1], cm7_3$overall[1], cm7_4$overall[1], cm7_5$overall[1]))

########################################
# 4 x 4 # 12 * 5 = 60 seconds = 1 minute
########################################
# 12 seconds
model4_1 <- naiveBayes(four, labels, subset = setdiff(1:nrow(training), 1*(1:indices)))
pred4_1 <- predict(model4_1, four[1*(1:indices),])
cm4_1 <- confusionMatrix(pred4_1, labels[1*(1:indices)]) # .4987

model4_2 <- naiveBayes(four, labels, subset = setdiff(1:nrow(training), 2*(1:indices)))
pred4_2 <- predict(model4_2, four[2*(1:indices),])
cm4_2 <- confusionMatrix(pred4_2, labels[2*(1:indices)])

model4_3 <- naiveBayes(four, labels, subset = setdiff(1:nrow(training), 3*(1:indices)))
pred4_3 <- predict(model4_3, four[3*(1:indices),])
cm4_3 <- confusionMatrix(pred4_3, labels[3*(1:indices)])

model4_4 <- naiveBayes(four, labels, subset = setdiff(1:nrow(training), 4*(1:indices)))
pred4_4 <- predict(model4_4, four[4*(1:indices),])
cm4_4 <- confusionMatrix(pred4_4, labels[4*(1:indices)])

model4_5 <- naiveBayes(four, labels, subset = setdiff(1:nrow(training), 5*(1:indices)))
pred4_5 <- predict(model4_5, four[5*(1:indices),])
cm4_5 <- confusionMatrix(pred4_5, labels[5*(1:indices)])

avgaccuracy4 <- mean(c(cm4_1$overall[1], cm4_2$overall[1], cm4_3$overall[1], cm4_4$overall[1], cm4_5$overall[1]))

# Compare average accuraies
avgaccuracy14; avgaccuracy7; avgaccuracy4;

elapsed <- proc.time() - start # 14 min