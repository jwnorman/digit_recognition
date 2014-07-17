Digit Recognition
=================

I worked on this project with two classmates, Won Joo Lee and Haomiao Meng. This project involves dimension reducing and machine learning techniques to predict a handwritten digit as 0 through 9. 

From MNIST, a handwritten digit database, we obtained a training dataset of 28,000 observations and a testing dataset of 14,000. Each observation included the handwritten digit (0-9) and 784 variables (28x28 pixels, 0-256 in darkness). 

I performed principal component analysis to determine how many components we were to use. This code can be found in pca.R. I also implemented my own dimension reduction technique, which I call Blocking or Chunking, which outperformed the principal component analysis on most machine learning techniques. This code can be found in Blocking.R. This involves merging the small pixels (28x28) into larger pixels, say 14x14 or 7x7, by summing the surrounding pixels. 

We researched and experimented with the following machine learning techniques: Linear Discriminant Analysis (LDA), Quadratic Discriminant Analysis (QDA), k-Nearest Neighbors (kNN), Random Forests, Support Vector Machines (SVM), and Naive Bayes. We used cross validation to find the best parameters for each technique. SVM, one of the methods that I was in charge of using, yielded the best error rate: it predicted incorrectly only 1.91% of the testing dataset.
