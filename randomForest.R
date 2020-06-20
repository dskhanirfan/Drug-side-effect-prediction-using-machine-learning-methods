
######################################################
######################################################
#Random Forest
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

diseaseindications1 <- as.data.frame(data[,6:730])
diseaseindications2 <- as.data.frame(data[,4572:4737])
diseaseindications <-as.data.frame(data[,6:730])
diseaseindications <- cbind(diseaseindications1, diseaseindications2)

row.names(diseaseindications) <-data[,2]
row.names(sideeffect) <- data[,2]

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

randomIterations = c(1606, 842,1408,1203,2016,1988,3000,4000,5000,6000)
am <- matrix(NA,numOfChunks,length(randomIterations))
pm <-matrix(NA,numOfChunks,length(randomIterations))
rm <-matrix(NA,numOfChunks,length(randomIterations))
fm <-matrix(NA,numOfChunks,length(randomIterations))


#r=1
for(r in 1:length(randomIterations)){
  set.seed(randomIterations[r])
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
  
  #chunks=1
  for(chunks in 1:numOfChunks){
    set.seed(randomIterations[r])
    randomIndicesClassZero = which(chunks==ChunkAssignments)
    randomIndicesClassOne<-sample(1:nrow(classOneValues), 30)
    
    classZeroValues[randomIndicesClassZero, ]
    classOneValues[randomIndicesClassOne, ]
    
    set.seed(randomIterations[r])
    l <- list(classZeroValues[randomIndicesClassZero, ],classOneValues[randomIndicesClassOne, ])
    dataset <- do.call(rbind, l)[order(sequence(sapply(l, nrow))), ] # interleave the two datasets
    dataset$sideeffect
    
    ############## momment hai bae momment ha
    
    numOfFolds = 10 #cv performance show
    numOfSamples = dim(dataset)[1]
    dataset$sideeffect
    set.seed(randomIterations[r])
    cvFolds = cut(seq(1,numOfSamples),breaks=numOfFolds,labels=FALSE)#sample(numOfFolds,numOfSamples,replace = T)
    #fold=1

    
    for(fold in 1:numOfFolds){
      if(fold==1){
        y.rf <- rep(NA, numOfSamples)
        #length(y.rf)
        #str(y.rf)
        
      }
      
      trainingIndex = which(fold!=cvFolds)
      testIndex = which(fold==cvFolds)
      
      Xtrain <- dataset[trainingIndex,]
      Xtrain = Xtrain[,-which(colSums(dataset[trainingIndex,])==0)]
      #dim(Xtrain)
      
      #sdFeatures = apply(Xtrain,2,sd,na.rm=T)
      #length(which(sdFeatures==0))
      
      Xtest = dataset[testIndex,]
      Xtest = Xtest[,-which(colSums(dataset[trainingIndex,])==0)]
      #dim(Xtest)
      #sdFeatures = apply(Xtest,2,sd,na.rm=T)
      #length(which(sdFeatures==0))
      
      train.result <- Xtrain[ ,"sideeffect"];
      test.result  <- Xtest[ , "sideeffect"];
      
      
      datasety <-  dataset[,"sideeffect"]
      #length(datasety)
      #str(datasety)
      
      #nrow(Xtrain)
      #formula <- as.formula(Xtrain$sideeffect ~ . );
      #dim(Xtrain)
      
      
      Xtrain[,"sideeffect"] = as.factor(Xtrain[,"sideeffect"])
      Xtest[,"sideeffect"] = as.factor(Xtest[,"sideeffect"])
      
      train.result <- Xtrain[ ,"sideeffect"];
      test.result  <- Xtest[ , "sideeffect"];
      
      
      library(randomForest)
      
      # allVars<- colnames(Xtrain)
      # predictorVars<-allVars[!allVars%in%"sideeffect"] 
      # predictorVars<- paste(predictorVars, collapse = "+")
      # formula <- as.formula(paste("sideeffect~", predictorVars, collapse = "+"));
      # 
      
      
      #formula <- as.formula(Xtrain$sideeffect ~ . );
      #rfmodel <- randomForest(formula, data = Xtrain)
      set.seed(randomIterations[r])
      bestmtry <- tuneRF(x= subset(Xtrain, select=-sideeffect), y=Xtrain$sideeffect, 
                        trace=F, plot=F, doBest=TRUE,do.trace=F)

      
      set.seed(randomIterations[r])
      # Fitting the model 
      rfmodel<-randomForest(x=subset(Xtrain, select=-sideeffect), y=Xtrain$sideeffect,
                             data = Xtrain, 
                    importance = T, mtry=bestmtry$mtry
                  )
      
      #bestmtry$forest
      #bestmtry$importance
      #index=as.numeric(which.min(bestmtry))
      
      
      #rfmodel
      #summary(rfmodel)
      
      
      
      #train.pred <- predict(rfmodel, Xtrain);
      test.pred <- predict(rfmodel, Xtest);
      
      
      
      
      # cat("\nEvaluation on training set:\n\n")
      # accuracy = sum(train.pred == Xtrain$sideeffect)/length(Xtrain$sideeffect)
      # precision = svm.table.train[1,1]/sum(svm.table.train[,1])
      # recall = svm.table.train[1,1]/sum(svm.table.train[1,])
      # f = 2 * (precision * recall) / (precision + recall)
      # cat(paste("Accuracy:\t", format(accuracy, digits=2), "\n",sep=" "))
      # cat(paste("Precision:\t", format(precision, digits=2), "\n",sep=" "))
      # cat(paste("Recall:\t\t", format(recall, digits=2), "\n",sep=" "))
      # cat(paste("F-measure:\t", format(f, digits=2), "\n",sep=" "))
      # 
      # cat("\nEvaluation on test set:\n\n")
      # accuracy = sum(test.pred == Xtest$sideeffect)/length(Xtest$sideeffect)
      # precision = svm.table.test[1,1]/sum(svm.table.test[,1])
      # recall = svm.table.test[1,1]/sum(svm.table.test[1,])
      # f = 2 * (precision * recall) / (precision + recall)
      # cat(paste("Accuracy:\t", format(accuracy, digits=2), "\n",sep=" "))
      # cat(paste("Precision:\t", format(precision, digits=2), "\n",sep=" "))
      # cat(paste("Recall:\t\t", format(recall, digits=2), "\n",sep=" "))
      # cat(paste("F-measure:\t", format(f, digits=2), "\n",sep=" "))
      # 
      
      
      y.rf[testIndex] <- test.pred
      #Xtest$sideeffect
      #datasety
      #levels(Xtest$sideeffect)
      
      #cat("\nY: :\n\n")
      #y.rf
      if(fold==10) {
        y.rf <- y.rf - 1
        
        library(AUC)  
        cvAUC=auc(roc(y.rf, as.factor(datasety)))
        

        cat("\n One Fold Data set Confusion matrix:\n")
        accuracy = cvAUC
        
        
        confusion_matrix <- table(pred = y.rf, true = datasety);
        confusion_matrix
        
        cat("\nEvaluation on Fold dataset:\n\n")
        #accuracy = sum(datasety == y.rf)/length(y.rf)
        precision = confusion_matrix[1,1]/sum(confusion_matrix[,1])
        recall = confusion_matrix[1,1]/sum(confusion_matrix[1,])
        f = 2 * (precision * recall) / (precision + recall)
        cat(paste("Accuracy:\t", format(accuracy, digits=2), "\n",sep=" "))
        cat(paste("Precision:\t", format(precision, digits=2), "\n",sep=" "))
        cat(paste("Recall:\t\t", format(recall, digits=2), "\n",sep=" "))
        cat(paste("F-measure:\t", format(f, digits=2), "\n",sep=" "))
        
        accuracy_matrix_full[chunks,fold] <-accuracy
        
        #f1=(2*tp)/(2*tp+fp+fn)
        
        #accuracy_matrix_full[chunks,fold] <- sum(datasety == y.rf)/length(y.rf)
        precision_matrix_full[chunks,fold]=confusion_matrix[1,1]/sum(confusion_matrix[,1])
        recall_matrix_full[chunks,fold]=confusion_matrix[1,1]/sum(confusion_matrix[1,])
        F_matrix_full[chunks,fold]=2 * (precision * recall) / (precision + recall)
        #Specificity_matrix_full[chunks,fold]=auc(specificity(y.rf, as.factor(datasety))
        #sensitivity_matrix_full[chunks,fold]=sensitivity(y.rf, as.factor(datasety))
        #accuracy_matrix_full
        #precision_matrix_full
        #recall_matrix_full
        #F_matrix_full
        
        
      }
    }
  }
 # 8217683003
  
  am[,r] <- accuracy_matrix_full[,10]
  pm[,r] <-precision_matrix_full[,10]
  rm[,r] <-recall_matrix_full[,10]
  fm[,r] <-F_matrix_full[,10]
  #spm[,r] <-Specificity_matrix_full[,10]
  #sem[,r] <-sensitivity_matrix_full[,10]
  
}
#62.29
#61.83
#colSums(accuracy_matrix_full[,10])
a <- rowMeans(t(am))
sd(a)
mean(a)

p <- rowMeans(t(pm))
r <- rowMeans(t(rm))
f <- rowMeans(t(fm))

#
rf <- cbind(a,p,r,f)
rf
sd(rf[,1])
sd(rf[,2])
sd(rf[,3])
sd(rf[,4])
#mean(a)
mean(rf[,1])
mean(rf[,2])
mean(rf[,3])
mean(rf[,4])

boxplot(rf[,1], main="rf  Accuracy")
boxplot(rf[,2], main="rf  Precision")
boxplot(rf[,3], main="rf  Recall")
boxplot(rf[,4], main="rf  F-measure")

#save(rf, file = "randomForestDIFP.RData")

