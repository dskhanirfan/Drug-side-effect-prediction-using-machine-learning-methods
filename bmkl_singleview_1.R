######################################################
#BEMKL "Bayesian Efficient Multiple Kernel Learning"
######################################################
library(KRLS)


###### Pairwise Distances ######
#####################################
pdist <- function(X1, X2) {
  if (identical(X1, X2) == TRUE) {
    D <- as.matrix(dist(X1))
  }
  else {
    D <- as.matrix(dist(rbind(X1, X2)))
    D <- D[1:nrow(X1), (nrow(X1) + 1):(nrow(X1) + nrow(X2))]
  }
  return(D)
}







# help(gausskernel)
# gausskernel <-
#   function(X=NULL,sigma=NULL)
#   {
#     return(exp(-1*as.matrix(dist(X)^2)/sigma))
#   }
# 

jaccard <- function(x1,x2) {
  # initialize similarity matrix
  if(isTRUE(all.equal(x1,x2))){
    m <- matrix(NA, nrow=ncol(x1),ncol=ncol(x1),dimnames=list(colnames(x1),colnames(x1)))
    tanimoto <- as.data.frame(m)
    
    for(i in 1:ncol(x1)) {
      for(j in i:ncol(x1)) {
        tanimoto[i,j]= length(which(x1[,i] & x1[,j])) / length(which(x1[,i] | x1[,j]))
        tanimoto[j,i]=tanimoto[i,j]        
      }
    }
    
    
  }
  else{
    m <- matrix(NA, nrow=ncol(x1),ncol=ncol(x2),dimnames=list(colnames(x1),colnames(x2)))
    tanimoto <- as.data.frame(m)
    
    for(i in 1:ncol(x1)) {
      for(j in 1:ncol(x2)) {
        tanimoto[i,j]= length(which(x1[,i] & x2[,j])) / length(which(x1[,i] | x2[,j]))
        
      }
    }
  }
  return(tanimoto)
}

source("../jintan/Downloads/thesisCodes/bemklmaster/bemkl_supervised_classification_variational_train.R")
source("../jintan/Downloads/thesisCodes/bemklmaster/bemkl_supervised_classification_variational_test.R")

data <- read.csv(file="/Users/jintan/9may.csv",header=T)

sideeffect <- as.data.frame(data[,"C0018681"])
#hist(data[,"C0018681"])
sideeffect[sideeffect>0.5] <- 1
sideeffect[sideeffect<0.5] <- -1

str(sideeffect)

diseaseIndicationsKernel <- as.data.frame(data[,6:730])
fingerprintsKernel <- as.data.frame(data[,4572:4737])

row.names(diseaseIndicationsKernel) <-data[,2]
row.names(fingerprintsKernel) <-data[,2]
row.names(sideeffect) <- data[,2]


colnames(sideeffect) <- c('sideeffect')

diseaseIndicationsKernel = diseaseIndicationsKernel[,-which(colSums(diseaseIndicationsKernel)==0)]
dim(diseaseIndicationsKernel)
diseaseIndicationsKernel <- na.omit(diseaseIndicationsKernel)
row.names(diseaseIndicationsKernel) <- data[,2]

fingerprintsKernel = fingerprintsKernel[,-which(colSums(fingerprintsKernel)==0)]
dim(fingerprintsKernel)
fingerprintsKernel <- na.omit(fingerprintsKernel)
#fingerprintsKernel <- cbind(fingerprintsKernel, sideeffect)
row.names(fingerprintsKernel) <- data[,2]


dfff <- cbind(diseaseIndicationsKernel, fingerprintsKernel)
dfff <- cbind(dfff, sideeffect)

classOneIndices <- which(dfff$sideeffect==1)
classZeroIndices <- which(dfff$sideeffect==-1)
classOneIndices
classZeroIndices

classOneValues <- dfff[classOneIndices,]
classZeroValues <- dfff[classZeroIndices,]


#View(classOneValues$sideeffect)
#View(classZeroValues$sideeffect)

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
    set.seed(randomIterations[r])
    randomIndicesClassZero = which(chunks==ChunkAssignments)
    randomIndicesClassOne<-sample(1:nrow(classOneValues), 30)
    
    classZeroValues[randomIndicesClassZero, ]
    classOneValues[randomIndicesClassOne, ]
    
    set.seed(randomIterations[r])
    #l <- list(a=classOneValues[randomIndicesClassOne, ],b=classZeroValues[randomIndicesClassZero, ])
    l <- list(classZeroValues[randomIndicesClassZero, ],classOneValues[randomIndicesClassOne, ])
    dataset <- do.call(rbind, l)[order(sequence(sapply(l, nrow))), ] # interleave the two datasets
    dataset$sideeffect
    dim(dataset)
    DIK <- dataset[,1:652]
    FPK <- dataset[,653:806]
    
    ############## momment hai bae momment ha
    
    numOfFolds = 10 #cv performance show
    numOfSamples = dim(dataset)[1]
    dataset$sideeffect
    set.seed(randomIterations[r])
    cvFolds = cut(seq(1,numOfSamples),breaks=numOfFolds,labels=FALSE)#sample(numOfFolds,numOfSamples,replace = T)
    #fold=1
    for(fold in 1:numOfFolds){
      if(fold==1){
        y.bemkl <- rep(0, numOfSamples)
        length(y.bemkl)
        str(y.bemkl)
        
      }
      
      set.seed(randomIterations[r])
      trainingIndex = which(fold!=cvFolds)
      testIndex = which(fold==cvFolds)
      
      set.seed(randomIterations[r])
      XtrainDIK <- DIK[trainingIndex,]
      XtrainDIK = XtrainDIK[,-which(colSums(DIK[trainingIndex,])==0)]
      dim(XtrainDIK)
      
      XtrainFPK <- FPK[trainingIndex,]
      XtrainFPK = XtrainFPK[,-which(colSums(FPK[trainingIndex,])==0)]
      dim(XtrainFPK)
      
      XtestDIK = DIK[testIndex,]
      XtestDIK = XtestDIK[,-which(colSums(DIK[trainingIndex,])==0)]
      dim(XtestDIK)
      
      XtestFPK = FPK[testIndex,]
      XtestFPK = XtestFPK[,-which(colSums(FPK[trainingIndex,])==0)]
      dim(XtestFPK)
      
      
      datasety <-  dataset[,"sideeffect"]
      length(datasety)
      str(datasety)
      
      
      
      #initalize the parameters of the algorithm
      parameters <- list()
      
      #set the hyperparameters of gamma prior used for sample weights
      #parameters$alpha_lambda <- 1e-10
      #parameters$beta_lambda <- 1e+10
      
      parameters$alpha_lambda <- 1
      parameters$beta_lambda <- 1
      
      #set the hyperparameters of gamma prior used for bias
      parameters$alpha_gamma <- 1
      parameters$beta_gamma <- 1
      
      #set the hyperparameters of gamma prior used for kernel weights
      parameters$alpha_omega <- 1
      parameters$beta_omega <- 1
      
      ### IMPORTANT ###
      #For gamma priors, you can experiment with three different (alpha, beta) values
      #(1, 1) => default priors
      #(1e-10, 1e+10) => good for obtaining sparsity
      #(1e-10, 1e-10) => good for small sample size problems
      
      #set the number of iterations
      parameters$iteration <- 200
      
      #set the margin parameter
      parameters$margin <- 1
      
      #determine whether you want to store the lower bound values
      parameters$progress <- 0
      
      #set the seed for random number generator used to initalize random variables
      parameters$seed <- randomIterations[r]
      
      #set the standard deviation of intermediate representations
      parameters$sigma_g <- 0.1
      
      
      #dim(XtrainDIK)
      #initialize the kernels and class labels for training
      
      Ktrain = array(0, dim=c(dim(XtrainDIK)[1],dim(XtrainDIK)[1],2))
      
      ################ Linear kernel#####################      TRAIN
      
      #tmp1= as.matrix(XtrainDIK) %*% t(as.matrix(XtrainDIK))
      #tmp2= as.matrix(XtrainFPK) %*% t(as.matrix(XtrainFPK))
      #dim(tmp2)
      
      ################ RADIAL kernel#####################      TRAIN      
      tmp1 <- pdist(as.matrix(XtrainDIK), as.matrix(XtrainDIK))
      tmp2 <- pdist(as.matrix(XtrainFPK), as.matrix(XtrainFPK))
      sigma1 <- mean(tmp1)
      sigma2 <- mean(tmp2)
      tmp1 <- exp(-tmp1^2 / (2 * sigma1^2))
      tmp2 <- exp(-tmp2^2 / (2 * sigma2^2))

      ################ JACCARD kernel####################      TRAIN            
      # 
      # 
      # tmp1=as.matrix(jaccard(t(as.matrix(XtrainDIK)), t(as.matrix(XtrainDIK))))
      # tmp1 <- as.matrix(tmp1)
      # tmp1[is.na(tmp1)] <- 0
      # for(i in 1:dim(tmp1)[1])
      # {
      #   tmp1[i,i] <- 1
      # }
      # 
      # 
      # tmp2=as.matrix(jaccard(t(as.matrix(XtrainFPK)), t(as.matrix(XtrainFPK))))
      # 
      # tmp2 <- as.matrix(tmp2)
      # tmp2[is.na(tmp2)] <- 0
      # for(i in 1:dim(tmp2)[1])
      # {
      #   tmp2[i,i] <- 1
      # }
      ############################################################################      
      Ktrain[1:dim(XtrainDIK)[1],1:dim(XtrainDIK)[1],1] <-  tmp1 #should be an Ntra x Ntra x P matrix containing similarity values between training samples
      #Ktrain[1:dim(XtrainDIK)[1],1:dim(XtrainDIK)[1],2] <-  tmp2 #should be an Ntra x Ntra x P matrix containing similarity values between training samples
      
      ytrain <- dataset[trainingIndex,"sideeffect"] #should be an Ntra x 1 matrix containing class labels (contains only -1s and +1s)
      
      
      #perform training
      state <- bemkl_supervised_classification_variational_train(Ktrain, ytrain, parameters)
      
      Ktest = array(0, dim=c(dim(XtrainDIK)[1],dim(XtestDIK)[1],2))
      
      
      ################ Linear kernel#####################      TEST      
      
      #tmp1= as.matrix(XtrainDIK) %*% t(as.matrix(XtestDIK))
      #tmp2= as.matrix(XtrainFPK) %*% t(as.matrix(XtestFPK))
      
      ################Radial kernel######################      TEST
      tmp1 <- pdist(as.matrix(XtrainDIK), as.matrix(XtestDIK))
      tmp2 <- pdist(as.matrix(XtrainFPK), as.matrix(XtestFPK))
      sigma1 <- mean(tmp1)
      sigma2 <- mean(tmp2)
      tmp1 <- exp(-tmp1^2 / (2 * sigma1^2))
      tmp2 <- exp(-tmp2^2 / (2 * sigma2^2))


      ################JACCARD KERNEL ###################       TEST      
      # 
      # tmp1=as.matrix(jaccard(t(as.matrix(XtrainDIK)), t(as.matrix(XtestDIK))))
      # tmp1 <- as.matrix(tmp1)
      # tmp1[is.na(tmp1)] <- 0
      # #K_test[,,m] <- tmp1
      # 
      # 
      # tmp2=as.matrix(jaccard(t(as.matrix(XtrainFPK)), t(as.matrix(XtestFPK))))
      # tmp2 <- as.matrix(tmp2)
      # tmp2[is.na(tmp2)] <- 0
      # 
      ##########################################################
      
      
      dim(tmp2)
      Ktest[1:dim(XtrainDIK)[1],1:dim(XtestDIK)[1],1] <-  tmp1 #should be an Ntra x Ntra x P matrix containing similarity values between training samples
      #Ktest[1:dim(XtrainDIK)[1],1:dim(XtestDIK)[1],2] <-  tmp2 #should be an Ntra x Ntra x P matrix containing similarity values between training samples
      
      prediction <- bemkl_supervised_classification_variational_test(Ktest, state)
      
      prediction$p
      #display the kernel weights
      #print(state$be$mu[-1])
      
      y.bemkl[testIndex] <- prediction$p
      
      if(fold==10) {
        #y.bemkl[y.bemkl>0.5] <- 1
        #y.bemkl[y.bemkl<1] <- -1
        #length(datasety)
        #y.bemkl = y.bemkl -1
        cat("\n One Fold Data set Confusion matrix:\n")
        confusion_matrix <- table(pred = y.bemkl, true = datasety);
        
        library(AUC)  
        cvAUC=auc(roc(y.bemkl, as.factor(datasety)))
        
        #confusion_matrix
        
        #library(caret)
        #cm <- confusionMatrix(y.bemkl,datasety)
        
        cat("\nEvaluation on Fold dataset:\n\n")
        #accuracy = sum(datasety == y.bemkl)/length(y.bemkl)
        accuracy = cvAUC
        precision = confusion_matrix[1,1]/sum(confusion_matrix[,1])
        recall = confusion_matrix[1,1]/sum(confusion_matrix[1,])
        f = 2 * (precision * recall) / (precision + recall)
        cat(paste("Accuracy:\t", format(accuracy, digits=2), "\n",sep=" "))
        cat(paste("Precision:\t", format(precision, digits=2), "\n",sep=" "))
        cat(paste("Recall:\t\t", format(recall, digits=2), "\n",sep=" "))
        cat(paste("F-measure:\t", format(f, digits=2), "\n",sep=" "))
        accuracy_matrix_full[chunks,fold] <-cvAUC
        #accuracy_matrix_full[chunks,fold] <- sum(datasety == y.bemkl)/length(y.bemkl)
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
bemkl <- cbind(a,p,r,f)
bemkl
sd(bemkl[,1])
sd(bemkl[,2])
sd(bemkl[,3])
sd(bemkl[,4])
mean(a)
mean(bemkl[,1])
mean(bemkl[,2])
mean(bemkl[,3])
mean(bemkl[,4])

boxplot(bemkl[,1], main="bemkl  Accuracy")
boxplot(bemkl[,2], main="bemkl  Precision")
boxplot(bemkl[,3], main="bemkl  Recall")
boxplot(bemkl[,4], main="bemkl  F-measure")

#save(bemkl, file = "bemklDIFP.RData")

