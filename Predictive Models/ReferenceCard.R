library(car)

#Define sest for prestige split
testidx <- which(1:nrow(Prestige)%%4==0)
prestige_train <- Prestige[-testidx,]
prestige_test <- Prestige[testidx,]


#Prestige split
testidx <- which(1:nrow(iris)%%5==0)
iris_train <- iris[-testidx,]
iris_test <- iris[testidx,]

##### LINEAR REGRESSION #####
model <- lm(data=prestige_train, prestige~.)
prediction <- predict(model, newdata=prestige_test) #predict using model
cor(prediction,prestige_test$prestige) #check correlation between results

summary(model) 

#features not marked with at least one * can be safely ignored
#In the example Education and income has a high influence to the prestige
#It is a common practice to identify those outliers, remove them, and then rerun the training

##### LOGISTIC REGRESSION #####
iris_train$isSetosa <- (iris_train$Species=="setosa") #define value to be predicted in the training set
formula <- isSetosa~Sepal.Length+Sepal.Width+Petal.Length+Petal.Width #define formula
logisticModel <- glm(formula, data=iris_train, family="binomial")
prob <- predict(logisticModel, newdata=iris_test, type="response") #predict values
iris_test$predictedSetosa <- round(prob,3)  #round predicted probabilities to 3 decimal digits
table(iris_test$predictedSetosa,iris_test$Species)  #check results matrix
summary(logisticModel)

##### REGRESSION WITH REGULARIZATION #####
library(glmnet)
cv.fit <- cv.glmnet(as.matrix(prestige_train[,c(-4, -6)]), 
                    as.vector(prestige_train[,4]), 
                    nlambda=100, alpha=0.7, family="gaussian")
plot(cv.fit)
coef(cv.fit)
#alpha controls if  regularization shall be done with ||Theta|| (aplha = 1) or (Theta)^2 (alpha = 0)
#in the above example "women" and "census" variables are discarded with respect to previous linear regression

##### NEURAL NETWORK #####
library(neuralnet)
nnet_iristrain <-iris_train

#Binarize the categorical output
nnet_iristrain <- cbind(nnet_iristrain, iris_train$Species == "setosa")
nnet_iristrain <- cbind(nnet_iristrain, iris_train$Species == "versicolor")
nnet_iristrain <- cbind(nnet_iristrain, iris_train$Species == "virginica")
names(nnet_iristrain)[6] <- "setosa"
names(nnet_iristrain)[7] <- "versicolor"
names(nnet_iristrain)[8] <- "virginica"

#define model with 3 hiddel layers
formula <- setosa+versicolor+virginica~Sepal.Length+Sepal.Width+Petal.Length+Petal.Width
nn <- neuralnet(formula, data=nnet_iristrain, hidden=c(3))
plot(nn)

#predict results
mypredict <- compute(nn, iris_test[-5])$net.result

# Consolidate multiple binary output back to categorical output
maxidx <- function(arr) {return(which(arr == max(arr)))}
idx <- apply(mypredict, 1, maxidx)
prediction <- c("setosa", "versicolor", "virginica")[idx]
table(prediction, iris_test$Species)

##### SUPPORT VECTOR MACHINE #####
#There are a couple of tuning parameters (ie. penalty and cost),
#so transformation is usually conducted in 2 steps-finding the optimal 
#parameter and then training the SVM model using that parameter

library(e1071)
#find optimal penalty (gamma) and cost
tune <- tune.svm(Species~., data=iris_train, gamma=10^(-6:-1), cost=10^(1:4))
summary(tune)

#train model with optimal parameters
bestGamma <- tune$best.parameters[1]
bestCost <- tune$best.parameters[2]
model <- svm(Species~., data=iris_train, method="C-classification",kernel="radial", probability=T, gamma=bestGamma, cost=bestCost)
prediction <- predict(model, iris_test, probability=T)
table(iris_test$Species, prediction)

##### BAYESIAN NETWORK AND NAIVE BAYES #####
library(e1071)
#Note: output must becategorical
model <- naiveBayes(Species~., data=iris_train)
prediction <- predict(model, iris_test[,-5])
table(prediction, iris_test[,5])

##### K-NEAREST NEIGHBOR #####
library(class)
train_input <- as.matrix(iris_train[,-5])
train_output <- as.vector(iris_train[,5])
test_input <- as.matrix(iris_test[,-5])
prediction <- knn(train_input, test_input, train_output, k=5)
table(prediction, iris_test$Species)

##### DECISION TREE #####
library(rpart)
#Train the decision tree
treemodel <- rpart(Species~., data=iris_train)
plot(treemodel,mar=c(1,1))
text(treemodel, use.n=T,cex=0.8)
#Predict using the decision tree
prediction <- predict(treemodel, newdata=iris_test, type="class")
#Use contingency table to see how accurate it is
table(prediction, iris_test$Species)

##### RANDOM FOREST - BAGGING APPROACH #####
#bagging models: select n training data out of N 
#at each decision node of the tree, 
#AND select m input features from the
#total M input features (m ~ sqrt(M))

library(randomForest)
#Train 500 trees, random selected attributes
model <- randomForest(Species~., data=iris_train, nTree=500)
#Predict using the forest
prediction <- predict(model, newdata=iris_test, type="class")
table(prediction, iris_test$Species)
importance(model)

##### RANDOM FOREST - BOOSTING APPROACH #####
#Samples the training data records. It puts more emphasis, 
#though, on the training data that
#is wrongly predicted in previous iterations. Initially, each training
#data is equally weighted. At each iteration, the data that is
#wrongly classified will have its weight increased
library(gbm)
iris2 <- iris
iris2$isVersicolor <- (iris2$Species=="versicolor") #define value to be predicted in the training set
iris2[45:55,]
formula <- isVersicolor ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width
model <- gbm(formula, data=iris2, n.trees=1000, interaction.depth=2,distribution="bernoulli")
prediction <- predict.gbm(model, iris2, type="response", n.trees=1000)
iris2$probVersicolor <- round(prediction, 3)
iris2$predVersicolor <- (iris2$probVersicolor > 0.5)
table(iris2$predVersicolor,iris2$Species)
summary(model)
