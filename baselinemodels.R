set.seed(123) 
# randomly pick 70% of the number of observations (365)
index <- sample(1:nrow(data[,4572:4737]),size = 0.7*nrow(data[,4572:4737])) 
#index <- sample(1:nrow(data[,6:731]),size = 0.7*nrow(data[,6:731])) 


xxxtrain <- X[index,] 
xxxtest <- X[-index,] 
View(xxxtest)
yyytrain <- Y[index,]

yyytest <- Y[-index,] 

nrow(xxxtrain)
nrow(xxxtest)


#################BASELINE
best.guess <- mean(yyytrain) 
# For the continuous outcome, the main error metric we will use to evaluate our 
# models is the RMSE (root mean squared error). This error measure gives more
# weight to larger residuals than smaller ones (a residual is the difference 
# between the predicted and the observed value). 

# Evaluate RMSE and MAE on the testing data
RMSE.baseline <- sqrt(mean((best.guess-yyytest)^2))
RMSE.baseline
#0.1698271


# We will use the MAE (mean absolute error) as a secondary error metric. It gives 
# equal weight to the residuals. One of the advantages of this error measure is 
# that it is easy to interpret: it tells us, on average, the magnitude of the error
# we get by using the model when compared to the actual observed values.
MAE.baseline <- mean(abs(best.guess-yyytest))
MAE.baseline
# 0.1127265
############################# Multiple linear regression

lin.reg <- lm(log(xxxtrain+1) ~ yyytrain, data = data)

summary(lin.reg)

#exp(lin.reg$coefficients["fp166"])
rrrrr <- data[,"C0018681"]
rrrrr[rrrrr>0] <- 1
scatterplot(rrrrr)
hist(rrrrr)
cor(data[,"D008080"],data[,"C0018681"])

test.pred.lin <- exp(predict(lin.reg,as.data.frame(yyytest)))-1


RMSE.lin.reg <- sqrt(mean((test.pred.lin-yyytest)^2))
#0.3143286
MAE.lin.reg <- mean(abs(test.pred.lin-yyytest))
#0.2307555

########################

# Needed to grow a tree
library(rpart)
# To draw a pretty tree (fancyRpartPlot function)
library(rattle)


rt <- rpart(yyytrain ~ as.matrix(xxxtrain), data=data)
plot(rt)
fancyRpartPlot(rt)
# As always, predict and evaluate on the test set
test.pred.rtree <- predict(rt,yyytest)

RMSE.rtree <- sqrt(mean((test.pred.rtree-yyytest)^2))
RMSE.rtree
MAE.rtree <- mean(abs(test.pred.rtree-yyytest))
MAE.rtree

printcp(rt)


rpart(yyytrain ~ as.matrix(xxxtrain), data = data)
min.xerror <- rt$cptable[which.min(rt$cptable[,"xerror"]),"CP"]
rt.pruned <- prune(rt,cp = min.xerror)
plot(rt.pruned)
test.pred.rtree.p <- predict(rt.pruned, yyytest)
RMSE.rtree.pruned <- sqrt(mean((test.pred.rtree.p-yyytest)^2))
RMSE.rtree.pruned
MAE.rtree.pruned <- mean(abs(test.pred.rtree.p-yyytest))
MAE.rtree.pruned



###############
install.packages("randomForest")
library(randomForest)
set.seed(123)

?randomForest
rf1 <- randomForest(dfff$sideeffect~., data = dfff, importance = TRUE, 
                    ntree=1000, na.action=na.exclude)
which.min(rf1$mse)


imp <- as.data.frame(sort(importance(rf1)[,1],decreasing = TRUE),optional = T)
names(imp) <- "% Inc MSE"
imp
test.pred.forest <- predict(rf,yyytest)
RMSE.forest <- sqrt(mean((test.pred.forest-yyytest)^2))
RMSE.forest

MAE.forest <- mean(abs(test.pred.forest-yyytest))
MAE.forest





accuracy <- data.frame(Method = c("Baseline","Linear Regression","Full tree","Pruned tree","Random forest"),
                       RMSE   = c(RMSE.baseline,RMSE.lin.reg,RMSE.rtree,RMSE.rtree.pruned,RMSE.forest),
                       MAE    = c(MAE.baseline,MAE.lin.reg,MAE.rtree,MAE.rtree.pruned,MAE.forest)) 

accuracy$RMSE <- round(accuracy$RMSE,2)
accuracy$MAE <- round(accuracy$MAE,2) 
accuracy

all.predictions <- data.frame(actual = yyytest,
                              baseline = best.guess,
                              linear.regression = test.pred.lin,
                              full.tree = test.pred.rtree,
                              pruned.tree = test.pred.rtree.p,
                              random.forest = test.pred.forest)


head(all.predictions)


library(tidyr)
all.predictions <- gather(all.predictions,key = model,value = predictions,2:6)
head(all.predictions)
tail (all.predictions)

# Predicted vs. actual for each model
ggplot(data = all.predictions,aes(x = actual, y = predictions)) + 
  geom_point(colour = "blue") + 
  geom_abline(intercept = 0, slope = 1, colour = "red") +
  geom_vline(xintercept = 23, colour = "green", linetype = "dashed") +
  facet_wrap(~ model,ncol = 2) + 
  coord_cartesian(xlim = c(0,70),ylim = c(0,70)) +
  ggtitle("Predicted vs. Actual, by model")


#https://www.r-bloggers.com/part-4a-modelling-predicting-the-amount-of-rain/


#################

library(randomForest)
?cv.lm
install.packages("DAAG")
library("DAAG")
set.seed(123)
gggg<- data[,"C0018681"]
gggg[gggg>0.5] <- 1
gggg[gggg<0.5] <- 0
hist(gggg)
oneIndices <- which(gggg==1)
zeroIndices <- which(gggg==0)




length(zeroIndices)/sum(length(zeroIndices)+length(oneIndices))

zeroData <- dfff[zeroIndices, ]

folds <- cut(seq(1,nrow(dfff)),breaks=10,labels=FALSE)





sideeffect <- as.data.frame(data[,"C0018681"])
hist(data[,"C0018681"])
#sideeffect[sideeffect>0.5] <- 1
sideeffect[sideeffect>0.5] <- 1
sideeffect[sideeffect<0.5] <- 0
sideeffect = as.factor(sideeffect)
sideeffect = as.factor(sideeffect)
str(sideeffect)
for(i in colnames(sideeffect)){
  sideeffect[,i] = as.factor(sideeffect[,i])
}


#View(sideeffect)
diseaseindications <- as.data.frame(data[,6:730])

row.names(diseaseindications) <- row.names(data)
row.names(sideeffect) <- row.names(data)

#diseaseindications <- as.data.frame(data[,4572:4737])
colnames(sideeffect) <- c('sideeffect')
Class2
dfff <- cbind(diseaseindications, sideeffect)


for(i in colnames(dfff)){
  dfff[,i] = as.factor(dfff[,i])
}
  str(dfff)
  class(dfff$sideeffect)
  
  nfold <- 5
  # assign folds evenly using the modulus operator
  fold0 <- sample.int(sum(sideeffect==0)) %% nfold
  fold1 <- sample.int(sum(sideeffect==1)) %% nfold
  foldid <- as.numeric(length(sideeffect))
  foldid[sideeffect==0] <- fold0
  foldid[sideeffect==1] <- fold1
  hist(foldid)
  which(foldid[sideeffect==0])
  
  #Randomly shuffle the data
  dfff<-dfff[sample(nrow(dfff)),]
  
  #Create 10 equally size folds
  folds <- cut(seq(1,nrow(dfff)),breaks=10,labels=FALSE)
  
  #Perform 10 fold cross validation
  for(i in 1:10){
    #Segement your data by fold using the which() function 
    testIndexes <- which(folds==i,arr.ind=TRUE)
    testData <- dfff[testIndexes, ]
    trainData <- dfff[-testIndexes, ]
    #Use the test and train data partitions however you desire...
  }
  
  
  
  ind<- caTools::sample.split(Y= dfff$sideeffect, SplitRatio  =0.7) 
  
  trainDF<- dfff[ind,] #467 observation
  testDF<- dfff[!ind,] #200 observation
  dfff <- as.data.frame(dfff)
  which(is.na(trainDF))
  which(is.na(testDF))
  sapply(trainDF, function(x) sum(is.na(x)))
  sapply(testDF, function(x) sum(is.na(x)))
  trainDF <- na.omit(trainDF)
  testDF <- na.omit(testDF)
  
  #to find the best mtry
  bestmtry <- tuneRF(trainDF, trainDF$sideeffect, ntreeTry = 100, stepFactor = 1, 
                     improve = 0.05, trace=T, plot=T, doBest=FALSE, na.action=na.exclude)
  bestmtry
  
  
  # Fitting the model 
  modelforest<-randomForest(formula =sideeffect~., data = trainDF,
                            importance = T, mtry=26, ntree=100,
                            proximity=T,  na.action=na.exclude)
  modelforest
  plot(modelforest, main = "error(Y) vs trees(X)")
  
  #Forest error rate 
  #OOB (Out of Bag rate)Misclassification Rate
  # Each tree is tested on 1/3rd of the no. of observations not used in 
  # building the tree
  
  
  # In random forest there is no need for cross-validation to get unbiased estimate of test set error. It is estimated internally during the run as follows.
  # Each tree is constructed using a different bootstrap sample from the original data. 
  # About one third of the cases are left out of the bootstrap sample and not used in the construction of the kth tree.
  # Put each case left out in the construction of the kth tree down the kth tree to get a classification.
  # In this way, a test set classification is obtained for each case in about one third of the trees.
  # At the end of the run, take j to be the class that got most of the votes every time case n was out of bag
  # The proportion of times that j is not qual to the true class of n averaged over all cases is the oob error estimate. This has proven to be unbiased in many tests.
  # 
  # The features of cv is inbuilt in random forest so there is no exclusive need to cross-validate
  # 
  
  
  #1. High strength of tree will have lower error [depends on mtry]
  #2. High correlation between trees increases the error [depends on mtree]
        # because each of the decision tree is not learning anything new they are almost the same copy of aech other. this
        # is because the mtry could be equal the number of predictors
    
  ### When we aare splitting in any tree we use gini or entroppy make a split. 
  #Plotting the importance of each variable
  # mean decrease accuracy = how much of model accuracy decreases if we drop the 
  # variable
  importance(modelforest)
  imp <- as.data.frame(sort(importance(modelforest)[,1],decreasing = TRUE),optional = T)
  names(imp) <- "% Inc MSE"
  imp
  
  intersect(which(dfff$D000152==1),which(dfff$sideeffect==1))
  intersect(which(dfff$D000152==0),which(dfff$sideeffect==0))
  
  
  intersect(which(dfff$D051437==1),which(dfff$sideeffect==1))
  intersect(which(dfff$D051437==0),which(dfff$sideeffect==0))
  
  intersect(which(data[,"D000152"]==0),which(data[,"C0018681"]==0))
  
  #left side graph tests how worse the model performs without each variable
  #Gini index captures how pure the nodes are at the end of the tree
  # if you remove the top var ...how much on an average gini decreases
  varImpPlot(modelforest, cex=0.7,
                   main = "Top 20 Important Disease Indications in Headache",
                   sort = T, n.var = 20)
  
  
  
  #which variables are actually used in the random forest
  varUsed(modelforest)
  
  # Partial Dependence plot gives graphical depiction of the 
  # marginal effect of variable on class probability (classification)
  # response (regression)
  partialPlot(modelforest, trainDF, D000152, "1")
  partialPlot(modelforest, trainDF, D051437, "1")
  partialPlot(modelforest, trainDF, D000163, "1")
  partialPlot(modelforest, trainDF, D015658, "1")
  
  # extract single tree
  # when it gives status -ive 1 it means the node is terminal, 
  # The prediction at the right column what is the outcome 
  # There is no left right daughter of the node
  plot(getTree(modelforest, 1, labelVar = TRUE))
  
  # Multi Dimensional Scaling plot of Proximity Matrix Plot the scaling 
  # coordinates of the proximity matrix from randomForest.
  # the two colors represent that there are two classes 0 and 1
  MDSplot(modelforest, trainDF$sideeffect)
  
    
    
  predictionswithclass <- predict(modelforest, testDF, type = 'class')
  t <- table(predictions=predictionswithclass, actual=testDF$sideeffect)
  
  #accuracy metric
  accuracyMetric<- sum(diag(t))/sum(t)
  accuracyMetric
  # plotting ROC curve and calculating AUC metric
  #install.packages("pROC")
  library(pROC)
  predictionWithProbs <- predict(modelforest, testDF, type = 'prob')
  auc<-  auc(testDF$sideeffect, predictionWithProbs[,2])
  plot(roc(testDF$sideeffect, predictionWithProbs[,2]))
  
  #Number of nodes for the trees
  hist(treesize(modelforest), main= "No. of nodes for the trees", col= "green")
#################### CROSS-VALIDATION 
  
  
  # Let's look into cross-validation using the caret package to see if we can get
  # more accurate estimates
  #install.packages("caret")
  #install.packages("doSNOW")
  library(caret)
  library(doSNOW)
  
  
  # Research has shown that 10-fold CV repeated 10 times is the best place to start,
  # however there are no hard and fast rules - this is where the experience of the 
  # Data Scientist (i.e., the "art") comes into play. We'll start with 10-fold CV,
  # repeated 10 times and see how it goes.
  
  
  # Leverage caret to create 100 total folds, but ensure that the ratio of those
  # that survived and perished in each fold matches the overall training set. This
  # is known as stratified cross validation and generally provides better results.
  #set.seed(123)
  inTraining <- createDataPartition(dfff$sideeffect, p = .75, list = FALSE)
  training <- dfff[ inTraining,]
  testing  <- dfff[-inTraining,]
  
  set.seed(825)
  gbmFit1 <- train(sideeffect ~ ., data = training, 
                   method = "gbm", 
                   trControl = fitControl,
                   ## This last option is actually one
                   ## for gbm() that passes through
                   verbose = FALSE)
  gbmFit1
  
  #install.packages("AppliedPredictiveModeling")
  library(AppliedPredictiveModeling)
  transparentTheme(trans = .4)
  library(caret)
  View(dfff)
  featurePlot(x = dfff, 
              y = dfff$sideeffect, 
              plot = "pairs",
              ## Add a key at the top
              auto.key = list(columns = 3)
              )
  
  featurePlot(x = dfff[, 1:4], 
              y = iris$Species, 
              plot = "ellipse",
              ## Add a key at the top
              auto.key = list(columns = 3))
  
  head(model.matrix(dfff ~ ., data = dfff))
  
  plot(testing)
  
  cv.10.folds <- createMultiFolds(dfff$sideeffect, k = 10, times = 5)
  ?createMultiFolds
  ?createDataPartition
  # Check stratification
  table(dfff$sideeffect)
  
  table(dfff$sideeffect[cv.10.folds[[3]]])
  table(dfff$sideeffect[cv.10.folds[[9]]])
  
  
  # Set seed for reproducibility and train
  set.seed(825)
  seeds <- vector(mode = "list", length = 51) # length is = (n_repeats*nresampling)+1
  for(i in 1:50) seeds[[i]]<- sample.int(n=1000, 38) # ...the number of tuning parameter...
  seeds[[51]]<-sample.int(1000, 1) # for the last model
  
  
  # Set up caret's trainControl object per above.
  ctrl.1 <- trainControl(method = "cv", number = 10, repeats = 5,
                         index = cv.10.folds, seeds=seeds)  
  
  levels(dfff$sideeffect) <- make.names(levels(factor(dfff$sideeffect)))
  # Set up doSNOW package for multi-core training. This is helpful as we're going
  # to be training a lot of trees.
  # NOTE - This works on Windows and Mac, unlike doMC
  cl <- makeCluster(4, type = "SOCK")
  registerDoSNOW(cl)
  
  prop.table(table(dfff$sideeffect))
  # Create model weights (they sum to one)
  model_weights <- ifelse(dfff$sideeffect == "X1",
                          (1/table(dfff$sideeffect)[1]) * 0.5,
                          (1/table(dfff$sideeffect)[2]) * 0.5)
  
  
  rf.5.cv.1 <- train(x = diseaseindications, y = dfff$sideeffect, method = "rf", tuneLength = 6,
                     weights = model_weights,   ntree = 100, trControl = ctrl.1)
  
  
  
  ctrl.1$seeds <- rf.5.cv.1$control$seeds
  
  # Build custom AUC function to extract AUC
  # from the caret model object
  test_roc <- function(model, data) {
    roc(data$Class,
        predict(model, data, type = "prob")[, "X1"])
  }
  
  rf.5.cv.1 %>%
    test_roc(data = diseaseindications) %>%
    auc()
  
  #Shutdown cluster
  stopCluster(cl)
  
  # Check out results
  rf.5.cv.1
  
  
  library(dplyr) # for data manipulation
  library(caret) # for model-building
  #install.packages("DMwR")
  library(DMwR) # for smote implementation
  library(purrr) # for functional programming (map)
  library(pROC) # for AUC calculations
  
  prop.table(table(dfff$sideeffect))
  
  set.seed(2969)
  # Set up control function for training
  
  make.names(c("0", "1"), unique = TRUE)
  
  ctrl <- trainControl(method = "repeatedcv",
                       number = 10,
                       repeats = 5,
                       summaryFunction = twoClassSummary,
                       classProbs = TRUE)
  
  # Build a standard classifier using a gradient boosted machine
  
  set.seed(5627)
  levels(dfff$sideeffect) <- make.names(levels(factor(dfff$sideeffect)))
  orig_fit <- train(dfff$sideeffect ~ .,
                    data = diseaseindications,
                    method = "gbm",
                    verbose = FALSE,
                    metric = "ROC",
                    trControl = ctrl)
  
  # Build custom AUC function to extract AUC
  # from the caret model object
  
  test_roc <- function(model, data) {
    
    roc(dfff$sideeffect,
        predict(model, dfff, type = "prob")[, "X2"])
    
  }
  install.packages("magrittr")
  library(magrittr)
  orig_fit %>%
    test_roc(data = dfff) %>%
    auc()
  
############### SVM #####################################
  
  
  # library(ggplot2)
  # qplot(dfff$D000152, dfff$D051437, data= dfff, color=sideeffect)
  
  
 #  
 #  we are looking for optimal  hyperplane to separate the two classes and we
 #  achieve that by maximizing the margin around the hyperplane. Points that lie
 #  on this boundry are called support vectors. This middle line is separating
 #  hyperplane. In situations where we are not able to obtain a linear separater 
 #  Data Points are projected into highre dimension space so that data poitns can 
 #  become linearly separable. For this projection we make use of Kernels. A program 
 #  that helps us in performing these activities we call this SVM. we use a library e1071
  
  #install.packages("e1071")
  library(e1071)
  # alternatively the traditional interface:
  xd <- subset(dfff, select = -sideeffect)
  yd <- sideeffect
  model <- svm(xd, yd) 
  mymodel <- svm(sideeffect~., data=dfff)
  #print(model)
  summary(model)
  
  # test with train data
  pred <- predict(model, xd)
  # (same as:)
  pred <- fitted(model)
  
  # Check accuracy:
  table(pred, yd)
  
  # compute decision values and probabilities:
  pred <- predict(model, xd, decision.values = TRUE)
  attr(pred, "decision.values")[1:4,]
  
  # visualize (classes by color, SV by crosses):
  plot(cmdscale(dist(iris[,-5])),
       col = as.integer(iris[,5]),
       pch = c("o","+")[1:150 %in% model$index + 1])