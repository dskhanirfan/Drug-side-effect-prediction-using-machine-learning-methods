
# Random Classifier
######################################################

data <- read.csv(file="/Users/jintan/9may.csv",header=T)

sideeffect <- as.data.frame(data[,"C0018681"])
#hist(data[,"C0018681"])
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
        y.rrrrrrr <- rep(NA, numOfSamples)
        length(y.rrrrrrr)
        str(y.rrrrrrr)
        
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
      

      x <- 0:1
      print(sample(x=c(0,1),size=length(testIndex),replace = TRUE, prob=c(0.5,0.5)))
      #print(rand(c(1,0), c(.50,.50)))
#      y.rrrrrrr[testIndex] <- sample(x=c(0,1),size=length(testIndex),replace = TRUE, prob=c(0.5,0.5))
      y.rrrrrrr[testIndex] <- sample(x=c(0,1),size=length(testIndex),replace = TRUE, prob=c(0.5,0.5))
      #?rand

      if(fold==10) {
        #hist(datasety)
        #hist(y.rrrrrrr)
        cat("\n One Fold Data set Confusion matrix:\n")
        confusion_matrix <- table(pred = y.rrrrrrr, true = datasety);
        confusion_matrix

        cat("\nEvaluation on Fold dataset:\n\n")
        accuracy = sum(datasety == y.rrrrrrr)/length(y.rrrrrrr)
        precision = confusion_matrix[1,1]/sum(confusion_matrix[,1])
        recall = confusion_matrix[1,1]/sum(confusion_matrix[1,])
        f = 2 * (precision * recall) / (precision + recall)
        cat(paste("Accuracy:\t", format(accuracy, digits=2), "\n",sep=" "))
        cat(paste("Precision:\t", format(precision, digits=2), "\n",sep=" "))
        cat(paste("Recall:\t\t", format(recall, digits=2), "\n",sep=" "))
        cat(paste("F-measure:\t", format(f, digits=2), "\n",sep=" "))

        accuracy_matrix_full[chunks,fold] <- sum(datasety == y.rrrrrrr)/length(y.rrrrrrr)
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
rrrrrrr <- cbind(a,p,r,f)
rrrrrrr
sd(rrrrrrr[,1])
sd(rrrrrrr[,2])
sd(rrrrrrr[,3])
sd(rrrrrrr[,4])
#mean(a)
mean(rrrrrrr[,1])
mean(rrrrrrr[,2])
mean(rrrrrrr[,3])
mean(rrrrrrr[,4])

boxplot(rrrrrrr[,1], main="Random Classifier Accuracy")
boxplot(rrrrrrr[,2], main="Random Classifier Precision")
boxplot(rrrrrrr[,3], main="Random Classifier Recall")
boxplot(rrrrrrr[,4], main="Random Classifier F-measure")

#save(rrrrrrr, file = "randomClassifierDIFP.RData")

