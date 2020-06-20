
######################################################
######################################################
#logistic regression
######################################################
######################################################
#https://datascienceplus.com/perform-logistic-regression-in-r/

#(1) Slicing our data into testing and training data sets, 
#(2) fit logistic regression model using the training data set, 
#(3) predict a categorical vairable from the fitted model using an "unseen" testing data, and
#(4) create the confusion matrix to compute the miss-classification error rate

data <- read.csv(file="/Users/jintan/9may.csv",header=T)

sideeffect <- as.data.frame(data[,"C0018681"])
hist(data[,"C0018681"])
sideeffect[sideeffect>0.5] <- 1
sideeffect[sideeffect<0.5] <- 0

str(sideeffect)

diseaseindications <- as.data.frame(data[,6:730])

row.names(diseaseindications) <- row.names(data)
row.names(sideeffect) <- row.names(data)

colnames(sideeffect) <- c('sideeffect')

diseaseindications = diseaseindications[,-which(colSums(diseaseindications)==0)]
dim(diseaseindications)
diseaseindications <- na.omit(diseaseindications)
dfff <- cbind(diseaseindications, sideeffect)


classOneIndices <- which(dfff$sideeffect==1)
classZeroIndices <- which(dfff$sideeffect==0)
classOneIndices
classZeroIndices

classOneValues <- dfff[classOneIndices,]
classZeroValues <- dfff[classZeroIndices,]


View(classOneValues$sideeffect)
View(classZeroValues$sideeffect)

nrow(classOneValues)
nrow(classZeroValues)

numOfChunks = 20

am <- matrix(NA,numOfChunks,length(randomIterations))
pm <-matrix(NA,numOfChunks,length(randomIterations))
rm <-matrix(NA,numOfChunks,length(randomIterations))
fm <-matrix(NA,numOfChunks,length(randomIterations))

randomIterations = c(1606, 842,1408,1203,2016,1988,3000,4000,5000,6000)

for(r in 1:length(randomIterations)){
  
  classZeroValues <- classZeroValues[sample(nrow(classZeroValues)),]
  nrow(classZeroValues)
  numOfClassZeroSamples = dim(classZeroValues)[1]
  
  set.seed(randomIterations)
  ChunkAssignments = cut(seq(1,numOfClassZeroSamples),breaks=numOfChunks,labels=FALSE)
  ChunkAssignments
  #chunks = 1
  numOfFolds = 10 #cv performance show
  accuracy_matrix_full=matrix(NA,numOfChunks,numOfFolds)
  precision_matrix_full=matrix(NA,numOfChunks,numOfFolds)
  recall_matrix_full=matrix(NA,numOfChunks,numOfFolds)
  F_matrix_full=matrix(NA,numOfChunks,numOfFolds)
  
  
  for(chunks in 1:numOfChunks){
    randomIndicesClassZero = which(chunks==ChunkAssignments)
    randomIndicesClassOne<-sample(1:nrow(classOneValues), 30)
    
    classZeroValues[randomIndicesClassZero, ]
    classOneValues[randomIndicesClassOne, ]
    
    l <- list(a=classZeroValues[randomIndicesClassZero, ],b=classOneValues[randomIndicesClassOne, ])
    dataset <- do.call(rbind, l)[order(sequence(sapply(l, nrow))), ] # interleave the two datasets
    dataset$sideeffect

    ############## momment hai bae momment ha
    
    numOfFolds = 10 #cv performance show
    numOfSamples = dim(dataset)[1]
    dataset$sideeffect
    set.seed(randomIterations)
    cvFolds = cut(seq(1,numOfSamples),breaks=numOfFolds,labels=FALSE)#sample(numOfFolds,numOfSamples,replace = T)
    
    for(fold in 1:numOfFolds){
      if(fold==1){
        y.lr <- rep(0, numOfSamples)
        length(y.lr)
        str(y.lr)
        
      }
      
      trainingIndex = which(fold!=cvFolds)
      testIndex = which(fold==cvFolds)
      
      Xtrain <- dataset[trainingIndex,]
      Xtrain = Xtrain[,-which(colSums(dataset[trainingIndex,])==0)]
      dim(Xtrain)
      
      #sdFeatures = apply(Xtrain,2,sd,na.rm=T)
      #length(which(sdFeatures==0))

      Xtest = dataset[testIndex,]
      Xtest = Xtest[,-which(colSums(dataset[trainingIndex,])==0)]
      dim(Xtest)
      #sdFeatures = apply(Xtest,2,sd,na.rm=T)
      #length(which(sdFeatures==0))
    
      train.result <- Xtrain[ ,"sideeffect"];
      test.result  <- Xtest[ , "sideeffect"];
      
      
      datasety <-  dataset[,"sideeffect"]
      length(datasety)
      str(datasety)
    
      nrow(Xtrain)
      formula <- as.formula(Xtrain$sideeffect ~ . );
      dim(Xtrain)

      logisticRegression <- glm(formula, data = Xtrain, 
                                family = binomial,
                                #prior=prior 
                                na.action=na.omit
      );
      summary(logisticRegression)

      train.pred <- predict(logisticRegression, Xtrain, type="response");
      test.pred <- predict(logisticRegression, Xtest, type="response");
      
      y.lr[testIndex] <- test.pred
      
      Xtest$sideeffect
      
      
      # cat("\nEvaluation on training set:\n\n")
      # accuracy = sum(train.pred == Xtrain$sideeffect)/length(Xtrain$sideeffect)
      # precision = svm.table.train[1,1]/sum(svm.table.train[,1])
      # recall = svm.table.train[1,1]/sum(svm.table.train[1,])
      # f = 2 * (precision * recall) / (precision + recall)
      # cat(paste("Accuracy:\t", format(accuracy, digits=2), "\n",sep=" "))
      # cat(paste("Precision:\t", format(precision, digits=2), "\n",sep=" "))
      # cat(paste("Recall:\t\t", format(recall, digits=2), "\n",sep=" "))
      # cat(paste("F-measure:\t", format(f, digits=2), "\n",sep=" "))
      
      cat("\nEvaluation on test set:\n\n")
      accuracy = sum(test.pred == Xtest$sideeffect)/length(Xtest$sideeffect)
      precision = svm.table.test[1,1]/sum(svm.table.test[,1])
      recall = svm.table.test[1,1]/sum(svm.table.test[1,])
      f = 2 * (precision * recall) / (precision + recall)
      cat(paste("Accuracy:\t", format(accuracy, digits=2), "\n",sep=" "))
      cat(paste("Precision:\t", format(precision, digits=2), "\n",sep=" "))
      cat(paste("Recall:\t\t", format(recall, digits=2), "\n",sep=" "))
      cat(paste("F-measure:\t", format(f, digits=2), "\n",sep=" "))
      
      #y.lr[testIndex] <- predict(lr.model, Xtest)$class
      Xtest$sideeffect
      datasety
      #levels(Xtest$sideeffect)
      
      if(fold==10) {
        y.lr[y.lr>0.5] <- 1
        y.lr[y.lr<1] <- 0
        
        cat("\n One Fold Data set Confusion matrix:\n")
        confusion_matrix <- table(pred = y.lr, true = datasety);
        confusion_matrix

        cat("\nEvaluation on Fold dataset:\n\n")
        accuracy = sum(datasety == y.lr)/length(y.lr)
        precision = confusion_matrix[1,1]/sum(confusion_matrix[,1])
        recall = confusion_matrix[1,1]/sum(confusion_matrix[1,])
        f = 2 * (precision * recall) / (precision + recall)
        cat(paste("Accuracy:\t", format(accuracy, digits=2), "\n",sep=" "))
        cat(paste("Precision:\t", format(precision, digits=2), "\n",sep=" "))
        cat(paste("Recall:\t\t", format(recall, digits=2), "\n",sep=" "))
        cat(paste("F-measure:\t", format(f, digits=2), "\n",sep=" "))

        accuracy_matrix_full[chunks,fold] <- sum(datasety == y.lr)/length(y.lr)
        precision_matrix_full[chunks,fold]=confusion_matrix[1,1]/sum(confusion_matrix[,1])
        recall_matrix_full[chunks,fold]=confusion_matrix[1,1]/sum(confusion_matrix[1,])
        F_matrix_full[chunks,fold]=2 * (precision * recall) / (precision + recall)
        accuracy_matrix_full
        precision_matrix_full
        recall_matrix_full
        F_matrix_full


      }
    }
  }

  
  am[,r] <- accuracy_matrix_full[,10]
  pm[,r] <-precision_matrix_full[,10]
  rm[,r] <-recall_matrix_full[,10]
  fm[,r] <-F_matrix_full[,10]
  
}

#colSums(accuracy_matrix_full[,10])
a <- rowMeans(t(am))
p <- rowMeans(t(pm))
r <- rowMeans(t(rm))
f <- rowMeans(t(fm))

#
lr <- cbind(a,p,r,f)
lr
sd(lr[,1])
sd(lr[,2])
sd(lr[,3])
sd(lr[,4])
#mean(a)
mean(lr[,1])
mean(lr[,2])
mean(lr[,3])
mean(lr[,4])

boxplot(lr[,1], main="lr Linear Accuracy")
boxplot(lr[,2], main="lr Linear Precision")
boxplot(lr[,3], main="lr Linear Recall")
boxplot(lr[,4], main="lr Linear F-measure")

#save(lr, file = "lr.RData")

