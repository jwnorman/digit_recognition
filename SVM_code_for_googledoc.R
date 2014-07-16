setwd("~/Desktop/Winter_2014/STA_135/Project/SVM")
print(load('~/Desktop/Winter_2014/STA_135/Project/SVM/digitsTrain.rda'))
print(load('~/Desktop/Winter_2014/STA_135/Project/SVM/digitsTest.rda'))
training <- sampleTrain
testing <- newTest

library("e1071")
library("caret")

fit1 <- svm(train, label, kernel = "radial", cost = 1, cross = 5)
fit2 <- svm(train, label, kernel = "radial", cost = 100, cross = 5)
fit3 <- svm(train, label, kernel = "radial", cost = 300, cross = 5)
fit4 <- svm(train, label, kernel = "radial", cost = 670, cross = 5)
fit5 <- svm(train, label, kernel = "radial", cost = 1000, cross = 5)
fit6 <- svm(train, label, kernel = "radial", cost = .25, cross = 5)
fit7 <- svm(train, label, kernel = "radial", cost = 100, cross = 5, gamma = 2^(-15)) 
fit8 <- svm(train, label, kernel = "radial", cost = 100, cross = 5, gamma = 2^(-10)) 
fit9 <- svm(train, label, kernel = "radial", cost = 100, cross = 5, gamma = 2^(-5)) 
fit12 <- svm(train, label, kernel = "linear", cost = 100, cross = 5)
fit13 <- svm(train, label, kernel = "polynomial", degree = 1, cost = 100, cross = 5)
fit14 <- svm(train, label, kernel = "polynomial", degree = 2, cost = 100, cross = 5)
fit15 <- svm(train, label, kernel = "polynomial", degree = 3, cost = 100, cross = 5)
fit17 <- svm(train, label, kernel = "sigmoid", cost = 100, cross = 5)
save(fit1, fit2, fit3, fit4, fit5, fit6, file = "5fitstest3_15_14.Rda")
save(fit7, fit8, fit9, fit12, fit13, fit14, fit15, fit17, file = "5fitstest3_17_14.Rda")

# Plot kernels against each other with default parameters
rad <- fit1$tot.accuracy
lin <- fit12$tot.accuracy
pol <- fit13$tot.accuracy
sig <- fit17$tot.accuracy
kerns <- rbind(rad, lin, pol, sig)
plot(kerns, col = c("red", "green", "blue", "orange"), pch = 16, cex = 3, axes = FALSE, xlab = "Kernel", ylab = "Average Percent Correctly Predicted", main = "Average Percent Correctly Predicted\nAfter 5-fold Cross Validation\nFor Different Kernels\nWith Other Parameters as Default")
axis(1, at=1:4, labels = c("Radial", "Linear", "Polynomial", "Sigmoid"))
axis(2, at=75:100)

# Plot gammas against each other with C = 100
gdefault <- fit2$tot.accuracy
g215 <- fit7$tot.accuracy
g210 <- fit8$tot.accuracy
g205 <- fit9$tot.accuracy
gammas <- rbind(g215, g210, g205, gdefault)
plot(gammas, col = c("red", "green", "blue", "orange"), pch = 2, lwd = 3, cex = 3, axes = FALSE, xlab = "Gamma Value", ylab = "Average Percent Correctly Predicted", main = "Average Percent Correctly Predicted\nAfter 5-fold Cross Validation\nFor Different Gamma Values")
axis(1, at=1:4, labels = c(expression(2^(-15)), expression(2^(-10)), expression(2^(-5)), expression(1/ncol(training))))
axis(2, at=75:110)

# Use on testing data
test <- as.matrix(testing[,-1]) %*% pc$loadings[,1:59]
#label <- as.factor(training[,1])
labels <- as.factor(testing[,1])
bestfit <- svm(train, label, kernel = "radial", cost = 100)
pred <- predict(bestfit, test)
confusionMatrix(pred, labels)
