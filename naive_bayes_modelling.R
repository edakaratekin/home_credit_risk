# Installing Packages
##install.packages("e1071")
##install.packages("caTools")
##install.packages("caret")

install.packages("klaR") 
# this is for 10-fold cv naive bayes

# Loading package
library(e1071)
library(caTools)
library(caret)


# Splitting data into train and test data
split <- sample.split(df, SplitRatio = 0.8)
train_cl <- subset(df, split == "TRUE")
test_cl <- subset(df, split == "FALSE")

# Feature Scaling
#train_scale <- scale(train_cl[, 1:4])
#test_scale <- scale(test_cl[, 1:4])

# Fitting Naive Bayes Model to training dataset
set.seed(120)  # Setting Seed
classifier_cl <- naiveBayes(TARGET ~ ., data = train_cl,laplace=1)
classifier_cl

# Predicting on test data'
y_pred <- predict(classifier_cl, newdata = test_cl)

# Confusion Matrix
cm <- table(test_cl$TARGET, y_pred)
cm

# Model Evaluation
confusionMatrix(cm)
recall(cm)
precision(cm)
F_meas(cm)


# naive bayes using 10-fold cross validation 
# reference https://rpubs.com/maulikpatel/224581

xTrain = train_cl[,-1] # removing y-outcome variable.
yTrain = train_cl$TARGET # only y.

xTest = test_cl[,-1]
yTest = test_cl$TARGET

model = train(xTrain,yTrain,'nb',trControl=trainControl(method='cv',number=10))

prop.table(table(predict(model$finalModel,xTest)$class,yTest)) # table() gives frequency table, prop.table() gives freq% table.
