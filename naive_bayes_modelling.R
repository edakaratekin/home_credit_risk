# Installing Packages
##install.packages("e1071")
##install.packages("caTools")
##install.packages("caret")

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
classifier_cl <- naiveBayes(TARGET ~ ., data = train_cl)
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
