setwd("/Users/diogocosta/Work/computer-science/cs3002/cs3002-laboratory-three")

accuracy = function(test, pred) {
  n = length(test) #the number of test cases
  ncorrect = sum(pred==test) #the number of correctly predicted
  accuracy=ncorrect/n
  
  return (accuracy)
}

confusion_matrix = function(test, pred) {
  return (table(test, pred))
}

winedata = read.csv('winedata3.csv', sep=",")
wineclass = winedata[,1]
winevalues = winedata[,-1]

#set up a training set
wineclassTrain = wineclass[1:100]
winevaluesTrain = winevalues[1:100,]

#and testset
wineclassTest = wineclass[100:178]
winevaluesTest = winevalues[100:178,]

library(rpart)
fit = rpart(wineclassTrain~., method="class", data=winevaluesTrain)

plot(fit, uniform=TRUE, main="Decision Tree for WineData3")
text(fit, use.n=TRUE, all=TRUE, cex=.8)

treepred = predict(fit, winevaluesTest, type = 'class')
print(accuracy(wineclassTest, treepred))
print(confusion_matrix(wineclassTest, treepred))

pfit = prune(fit, cp=0.1)
plot(pfit, uniform=TRUE, main="Pruned Decision Tree for WineData3")
text(pfit, use.n=TRUE, all=TRUE, cex=.8)

library(class)

knn3pred = knn(winevaluesTrain, winevaluesTest, wineclassTrain, k=3)
print(accuracy(wineclassTest, knn3pred))
print(confusion_matrix(wineclassTest, knn3pred))

# ASSESSED EXERCISE
raw_data = read.csv('iris.csv', sep=",", header = FALSE)
class_data = read.csv('iris_real.csv', sep=",", header = FALSE)

split = sample(150, 100)

x_train = raw_data[split,]
x_test = raw_data[-split,]

y_train = class_data[split,]
y_test = class_data[-split,]

fit = rpart(y_train~., method="class", data=x_train)
fit = prune(fit, cp=0.01)
plot(fit, uniform=TRUE, main="Decision Tree for Iris Data")
text(fit, use.n=TRUE, all=TRUE, cex=.8)

treepred <-predict(fit, x_test, type = 'class')
print(accuracy(y_test, treepred))
print(confusion_matrix(y_test, treepred))

two_raw = raw_data[, c('V1', 'V2')]
plot(two_raw[, c('V1')], two_raw[, c('V2')], col = c("blue","red"), main='Scatter Plot for V1 and V2', xlab = 'V1', ylab = 'V2')

two_raw_sample = sample(150, 100)

two_raw_x_train = two_raw[two_raw_sample,]
two_raw_x_test = two_raw[-two_raw_sample,]

two_classes_y_train = class_data[two_raw_sample,]
two_classes_y_test = class_data[-two_raw_sample,]

fit = rpart(two_classes_y_train~., method="class", data=two_raw_x_train)
plot(fit, uniform=TRUE, main="Decision Tree for Iris Columns V1 and V2")
text(fit, use.n=TRUE, all=TRUE, cex=.8)

treepred <-predict(fit, two_raw_x_test, type = 'class')
print(accuracy(two_classes_y_test, treepred))
print(confusion_matrix(two_classes_y_test, treepred))

kmeans <- c(3, 5, 7, 9)
for (i in kmeans) {
  knn3pred = knn(two_raw_x_train, two_raw_x_test, two_classes_y_train, k=i)
  print(paste("KMeans:", i, accuracy(two_classes_y_test, knn3pred)))
  print(confusion_matrix(two_classes_y_test, knn3pred))
}
