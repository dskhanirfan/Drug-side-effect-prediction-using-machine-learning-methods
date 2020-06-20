
######################################################
######################################################
#Neural Networks
######################################################
######################################################
#https://www.youtube.com/watch?v=-Vs9Vae2KI0
#Advantage: Robust with noisy data
#Disadvantage: is that it takes loger time to train
#less interpretable than other models such as decision trees
#Default Algorithm: rpop+

#(1) Slicing our data into testing and training data sets, 
#(2) fit logistic regression model using the training data set, 
#(3) predict a categorical vairable from the fitted model using an "unseen" testing data, and
#(4) create the confusion matrix to compute the miss-classification error rate

data <- read.csv(file="/Users/jintan/9may.csv",header=T)

sideeffect <- as.data.frame(data[,"C0018681"])
#hist(data[,"C0018681"])
sideeffect[sideeffect>0.5] <- 1
sideeffect[sideeffect<0.5] <- 0

str(sideeffect)

#diseaseindications <- as.data.frame(data[,6:730])

#diseaseindications1 <- as.data.frame(data[,6:730])
diseaseindications <- as.data.frame(data[,4572:4737])

#diseaseindications <- cbind(diseaseindications1, diseaseindications2)

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
  set.seed(randomIterations[r])
  classZeroValues <- classZeroValues[sample(nrow(classZeroValues)),]
  nrow(classZeroValues)
  numOfClassZeroSamples = dim(classZeroValues)[1]
  
  set.seed(randomIterations[r])
  #print(set.seed(randomIterations))
  ChunkAssignments = cut(seq(1,numOfClassZeroSamples),breaks=numOfChunks,labels=FALSE)
  ChunkAssignments
  #chunks = 1
  numOfFolds = 10 #cv performance show
  accuracy_matrix_full=matrix(NA,numOfChunks,numOfFolds)
  precision_matrix_full=matrix(NA,numOfChunks,numOfFolds)
  recall_matrix_full=matrix(NA,numOfChunks,numOfFolds)
  F_matrix_full=matrix(NA,numOfChunks,numOfFolds)
  
  
  for(chunks in 1:numOfChunks){
    set.seed(randomIterations[r])
    randomIndicesClassZero = which(chunks==ChunkAssignments)
    randomIndicesClassOne<-sample(1:nrow(classOneValues), 30)
    
    classZeroValues[randomIndicesClassZero, ]
    classOneValues[randomIndicesClassOne, ]
    
    set.seed(randomIterations[r])
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
        y.nn <- rep(NA, numOfSamples)
        length(y.nn)
        str(y.nn)
        
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
      
      #Xtrain[,"sideeffect"] = as.factor(Xtrain[,"sideeffect"])
      #Xtest[,"sideeffect"] = as.factor(Xtest[,"sideeffect"])
      
      #install.packages("neuralnet")
      #library(nnet)
      library(neuralnet)
      allVars<- colnames(Xtrain)
      predictorVars<-allVars[!allVars%in%"sideeffect"] 
      predictorVars<- paste(predictorVars, collapse = "+")
      formula <- as.formula(paste("sideeffect~", predictorVars, collapse = "+"));
      dim(Xtrain)
      
      class(Xtest$sideeffect)
      #library(naivebayes)
      #install.packages("naivebayes")
      
      #x <- subset(Xtrain, select=-sideeffect)
      #y <- Xtrain$sideeffect
      #nn.model <- naiveBayes(x, y, data = Xtrain)
      set.seed(randomIterations[r])
      neuralmodel <-neuralnet(formula=formula, 
                               data=Xtrain,
                               hidden = c(30,10),
                               act.fct = "logistic",
                               linear.output = FALSE
                               #err.fct = "ce",
                               #lifesign = 'full',
                               #rep=5,
                               #algorithm =  'rprop+',
                               #stepmax = 100000
                               )
     # ?neuralnet
      summary(neuralmodel)
      #plot(neuralmodel)
      test.pred <- compute(neuralmodel, subset(Xtest, select=-sideeffect))
      p1 <- test.pred$net.result
      #pred1 <- ifelse(p1>0.5,1,0)
      tab1 <- table(p1, Xtest$sideeffect)
      tab1
      
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

      y.nn[testIndex] <- p1[,1]
      
      
      
      # neuralmodel <-nnet(formula=formula,x=x, y=y, size = 5, 
     #                         data=Xtrain)
      #nn.model <-naive_bayes(formula, Xtrain)
      
      
      #nn.model <- naiveBayes(x, y, data = Xtrain)
      #class(neuralmodel)
      
      
      #predict(neuralmodel, Xtest, type="class")
      
      #predict(nn.model, newdata = Xtest, type = c("class","prob"))
      
      
      
      #train.pred <- predict(nn.model, Xtrain);
      #test.pred <-  predict(nn.model, Xtest);
      
      #y.nn[testIndex] <- predict(nn.model, Xtest)
      #y.nn[testIndex] <- test.pred
      #y.nn[testIndex] <- test.pred
      #test.pred$net.result
      #Xtest$sideeffect
      
      
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
      #y.nn[testIndex] <- predict(nn.model, Xtest)$class
      #Xtest$sideeffect
      #datasety
      #levels(Xtest$sideeffect)
      
      if(fold==10) {
        #y.nn[y.nn>0.5] <- 1
        #y.nn[y.nn<1] <- 0
        #y.nn <- y.nn -1
        
        library(AUC)  
        cvAUC=auc(roc(y.nn, as.factor(datasety)))
        
        
        cat("\n One Fold Data set Confusion matrix:\n")
        accuracy = cvAUC
        
       # cat("\n One Fold Data set Confusion matrix:\n")
        
        confusion_matrix <- table(pred = y.nn, true = datasety);
        confusion_matrix
        
        cat("\nEvaluation on Fold dataset:\n\n")
        #accuracy = sum(datasety == y.nn)/length(y.nn)
        precision = confusion_matrix[1,1]/sum(confusion_matrix[,1])
        recall = confusion_matrix[1,1]/sum(confusion_matrix[1,])
        f = 2 * (precision * recall) / (precision + recall)
        cat(paste("Accuracy:\t", format(accuracy, digits=2), "\n",sep=" "))
        cat(paste("Precision:\t", format(precision, digits=2), "\n",sep=" "))
        cat(paste("Recall:\t\t", format(recall, digits=2), "\n",sep=" "))
        cat(paste("F-measure:\t", format(f, digits=2), "\n",sep=" "))
        
        #accuracy_matrix_full[chunks,fold] <- sum(datasety == y.nn)/length(y.nn)
        accuracy_matrix_full[chunks,fold] <- accuracy
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

sd(a)
mean(a)
#dfff
nn_fingerprintsF <- cbind(a,p,r,f)
nn
sd(nn_fingerprintsF[,1])
sd(nn[,2])
sd(nn[,3])
sd(nn[,4])
#mean(a)
mean(nn_fingerprintsF[,1])
mean(nn[,2])
mean(nn[,3])
mean(nn[,4])

boxplot(nn[,1], main="nn Linear Accuracy")
boxplot(nn[,2], main="nn Linear Precision")
boxplot(nn[,3], main="nn Linear Recall")
boxplot(nn[,4], main="nn Linear F-measure")
nnFP_2 <- nn
save(nn_fingerprintsF, file = "nn_fingerprintsF.RData")
dfff
https://www.r-bloggers.com/multilabel-classification-with-neuralnet-package/