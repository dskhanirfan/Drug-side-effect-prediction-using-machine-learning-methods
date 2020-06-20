######################################################
#NaiveBayes
######################################################

data <- read.csv(file="/Users/jintan/9may.csv",header=T)

sideeffect <- as.data.frame(data[,"C0018681"])
hist(data[,"C0018681"])
sideeffect[sideeffect>0.5] <- 1
sideeffect[sideeffect<0.5] <- 0

str(sideeffect)

diseaseindications1 <- as.data.frame(data[,6:730])
diseaseindications2 <- as.data.frame(data[,4572:4737])

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

for(r in 1:length(randomIterations)){
  #r=1
  classZeroValues <- classZeroValues[sample(nrow(classZeroValues)),]
  nrow(classZeroValues)
  numOfClassZeroSamples = dim(classZeroValues)[1]
  
  set.seed(randomIterations[r])
  ChunkAssignments = cut(seq(1,numOfClassZeroSamples),breaks=numOfChunks,labels=FALSE)
  ChunkAssignments
  #chunks = 1
  numOfFolds = 10 #cv performance show
  accuracy_matrix_full=matrix(NA,numOfChunks,numOfFolds)
  precision_matrix_full=matrix(NA,numOfChunks,numOfFolds)
  recall_matrix_full=matrix(NA,numOfChunks,numOfFolds)
  F_matrix_full=matrix(NA,numOfChunks,numOfFolds)
  
  
  for(chunks in 1:numOfChunks){
    #chunks=1
    randomIndicesClassZero = which(chunks==ChunkAssignments)
    randomIndicesClassOne<-sample(1:nrow(classOneValues), 30)
    
    classZeroValues[randomIndicesClassZero, ]
    classOneValues[randomIndicesClassOne, ]
    
    #l <- list(a=classOneValues[randomIndicesClassOne, ],b=classZeroValues[randomIndicesClassZero, ])
    l <- list(a=classZeroValues[randomIndicesClassZero, ],b=classOneValues[randomIndicesClassOne, ])
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
        y.nB <- rep(0, numOfSamples)
        length(y.nB)
        str(y.nB)
        
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
      
      
      Xtrain[,"sideeffect"] = as.factor(Xtrain[,"sideeffect"])
      Xtest[,"sideeffect"] = as.factor(Xtest[,"sideeffect"])
      
      
      train.result <- Xtrain[ ,"sideeffect"];
      test.result  <- Xtest[ , "sideeffect"];
      
      
      datasety <-  dataset[,"sideeffect"]
      length(datasety)
      str(datasety)
      
      nrow(Xtrain)
      
      library(e1071)
      dim(Xtrain)
      # allVars<- colnames(Xtrain)
      # predictorVars<-allVars[!allVars%in%"sideeffect"] 
      # predictorVars<- paste(predictorVars, collapse = "+")
      # formula <- as.formula(paste("sideeffect~", predictorVars, collapse = "+"));
      
      set.seed(randomIterations[r])
      nB.model <- naiveBayes(subset(Xtrain, select=-sideeffect),
                             Xtrain$sideeffect,
                             data = Xtrain, 
                             laplace = 1,
                             na.action=na.omit
      );
      #summary(nB.model)
      
      test.pred <- predict(nB.model,
                            subset(Xtest, select=-sideeffect)
                           );
      
      #summary(test.pred)
      
      y.nB[testIndex] <- test.pred
      
      Xtest$sideeffect
      

      
      if(fold==10) {
        #y.nB[y.nB>0.5] <- 1
        #y.nB[y.nB<1] <- 0
        
        y.nB = y.nB -1
        cat("\n One Fold Data set Confusion matrix:\n")
        confusion_matrix <- table(pred = y.nB, true = datasety);
        confusion_matrix
        
        #library(caret)
        #cm <- confusionMatrix(y.nB,datasety)
        
        cat("\nEvaluation on Fold dataset:\n\n")
        accuracy = sum(datasety == y.nB)/length(y.nB)
        precision = confusion_matrix[1,1]/sum(confusion_matrix[,1])
        recall = confusion_matrix[1,1]/sum(confusion_matrix[1,])
        f = 2 * (precision * recall) / (precision + recall)
        cat(paste("Accuracy:\t", format(accuracy, digits=2), "\n",sep=" "))
        cat(paste("Precision:\t", format(precision, digits=2), "\n",sep=" "))
        cat(paste("Recall:\t\t", format(recall, digits=2), "\n",sep=" "))
        cat(paste("F-measure:\t", format(f, digits=2), "\n",sep=" "))
        
        accuracy_matrix_full[chunks,fold] <- sum(datasety == y.nB)/length(y.nB)
        precision_matrix_full[chunks,fold]=confusion_matrix[1,1]/sum(confusion_matrix[,1])
        recall_matrix_full[chunks,fold]=confusion_matrix[1,1]/sum(confusion_matrix[1,])
        F_matrix_full[chunks,fold]=2 * (precision * recall) / (precision + recall)
        #Kappa_matrix_full[chunks,fold] = cm$overall[2]
        #P-Value[chunks,fold]= cm$overall[6]
        
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
nB <- cbind(a,p,r,f)
nB
sd(nB[,1])
sd(nB[,2])
sd(nB[,3])
sd(nB[,4])
#mean(a)
mean(nB[,1])
mean(nB[,2])
mean(nB[,3])
mean(nB[,4])

boxplot(nB[,1], main="nB  Accuracy")
boxplot(nB[,2], main="nB  Precision")
boxplot(nB[,3], main="nB  Recall")
boxplot(nB[,4], main="nB  F-measure")

#save(nB, file = "nBDIFP.RData")

