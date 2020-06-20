
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

class(jaccard) <- "kernel"

#install.packages("klaR")
#library(klaR)
#??klaR
#??KRLS
library("kernlab")
#??kernlab

#help()

data <- read.csv(file="/Users/jintan/9may.csv",header=T)

sideeffect <- as.data.frame(data[,"C0018681"])
#hist(data[,"C0018681"])
sideeffect[sideeffect>0.5] <- 1
sideeffect[sideeffect<0.5] <- 0

str(sideeffect)

#View(sideeffect)
diseaseindications <- as.data.frame(data[,6:730])
#diseaseindications1 <- as.data.frame(data[,6:730])
#diseaseindications2 <- as.data.frame(data[,4572:4737])
#diseaseindications <- as.data.frame(data[,4572:4737])

#diseaseindications <- cbind(diseaseindications1, diseaseindications2)

row.names(diseaseindications) <- data[,2]
row.names(sideeffect) <- data[,2]

#diseaseindications <- as.data.frame(data[,4572:4737])
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

nrow(classOneValues)
nrow(classZeroValues)

numOfChunks = 20
randomIterations = c(1606, 842,1408,1203,2016,1988,3000,4000,5000,6000)

am <- matrix(NA,numOfChunks,length(randomIterations))
pm <-matrix(NA,numOfChunks,length(randomIterations))
rm <-matrix(NA,numOfChunks,length(randomIterations))
fm <-matrix(NA,numOfChunks,length(randomIterations))


for(r in 1:length(randomIterations)){
  # r=1
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
    #trainingIndex = which(fold!=cvFolds)
    randomIndicesClassZero = which(chunks==ChunkAssignments)
    #length(randomIndicesClassZero)
    #randomIndicesClassZero<-sample(1:nrow(classZeroValues), 25)
    randomIndicesClassOne<-sample(1:nrow(classOneValues), 30)
    
    classZeroValues[randomIndicesClassZero, ]
    classOneValues[randomIndicesClassOne, ]
    
    set.seed(randomIterations[r])
    #dataset <- rbind(classZeroValues[randomIndicesClassZero, ], classOneValues[randomIndicesClassOne, ])
    l <- list(classZeroValues[randomIndicesClassZero, ],classOneValues[randomIndicesClassOne, ])
    dataset <- do.call(rbind, l)[order(sequence(sapply(l, nrow))), ] # interleave the two datasets
    dataset$sideeffect
    
    #View(dataset$sideeffect)
    #hist(as.numeric(dataset$sideeffect))
    #}
    
    ############## momment hai bae momment ha
    
    numOfFolds = 10 #cv performance show
    numOfSamples = dim(dataset)[1]
    dataset$sideeffect
    set.seed(randomIterations[r])
    cvFolds = cut(seq(1,numOfSamples),breaks=numOfFolds,labels=FALSE)#sample(numOfFolds,numOfSamples,replace = T)
    
    #trainingIndex = which(fold!=cvFolds)
    #testIndex = which(fold==cvFolds)
    #fold = 1
    #mse=matrix(NA,length(alphas),1)
    #spCor=matrix(NA,length(alphas),1)
    #prCor=matrix(NA,length(alphas),1)
    # y.svm <- rep(NA, numOfSamples)
    # length(y.svm)
    # str(y.svm)
    for(fold in 1:numOfFolds){
      #fold=1  
      if(fold==1){
        y.svm <- rep(NA, numOfSamples)
        length(y.svm)
        str(y.svm)
        
      }
      
      
      trainingIndex = which(fold!=cvFolds)
      testIndex = which(fold==cvFolds)
      
      
      Xtrain <- dataset[trainingIndex,]
      #Xtrain = Xtrain[,-which(colSums(dataset[trainingIndex,])==0)]
      dim(Xtrain)
      Xtest = dataset[testIndex,]
      #Xtest = Xtest[,-which(colSums(dataset[trainingIndex,])==0)]
      dim(Xtest)
    
      #train.result <- Xtrain[ ,"sideeffect"];
      #test.result  <- Xtest[ , "sideeffect"];
      
      datasety <-  dataset[,"sideeffect"]
      datasety <- as.factor(datasety)
      class(datasety)
      length(datasety)
      str(datasety)
      
      numOfinnerFolds = 10 #inner cv performance show
      numOfinnerSamples = dim(Xtrain)[1]
      Xtrain$sideeffect
      
      set.seed(randomIterations[r])      
      innercvFolds = cut(seq(1,numOfinnerSamples),breaks=numOfinnerFolds,labels=FALSE)
      #innercvFolds = sample(numOfinnerFolds,length(Xtrain$sideeffect),replace = T)
      length(innercvFolds)
      inner_test_accuracy_matrix=matrix(NA,3)
      #innercvFolds = sample(numOfFolds,dim(Ytrain)[1],replace = T)
      set.seed(randomIterations[r])
      #innercvFolds = cut(seq(1,length(Xtrain$sideeffect)),breaks=numOfinnerFolds,labels=FALSE)
      #innerfold=1
      costcounter=c(1,5,10)
      
      for(i in 1:3){
        
        for(innerfold in 1:numOfinnerFolds){
          #innerfold=1
          if(innerfold==1){
            y.innersvm <- rep(NA, length(innercvFolds))
            length(y.innersvm)
            str(y.innersvm)
            
          }
          
          innertrainingIndex = which(innerfold!=innercvFolds)
          innertestIndex = which(innerfold==innercvFolds)
          
          
          innerXtrain <- Xtrain[innertrainingIndex,]
          #class(innerXtrain)
          dim(innerXtrain)
          dim(Xtrain)
          length(which(colSums(Xtrain[innertrainingIndex,])==0))
          innerXtrain = Xtrain[innertrainingIndex,-which(colSums(Xtrain[innertrainingIndex,])==0)]
          dim(innerXtrain)
          innerXtest = Xtrain[innertestIndex,]  
          which(colSums(Xtrain[innertrainingIndex,])==0)
          innerXtest = Xtrain[innertestIndex,-which(colSums(Xtrain[innertrainingIndex,])==0)]
          dim(innerXtest)
          
          dim(Xtrain)
          dim(Xtest)
          
          innerxtr <- subset(innerXtrain, select=-sideeffect)
          #ytr <- as.factor(Xtrain$sideeffect)
          innerytr <- innerXtrain$sideeffect
          innerxtr = na.omit(innerxtr)
          innerytr = na.omit(innerytr)
          
          innerxte <- subset(innerXtest, select=-sideeffect)
          #yte <- as.factor(Xtest$sideeffect)
          inneryte <- innerXtest$sideeffect
          innerxte = na.omit(innerxte)
          inneryte = na.omit(inneryte)
          
          
          set.seed(randomIterations[r])
          ## S4 method for signature 'matrix'
          
          innertmp1 <- jaccard(t(as.matrix(innerxtr)), t(as.matrix(innerxtr)))
          innertmp1 <- as.matrix(innertmp1)
          innertmp1[is.na(innertmp1)] <- 0
          dim(innertmp1)
          innertmp2 <- jaccard(t(as.matrix(innerxte)), t(as.matrix(innerxtr)))
          innertmp2 <- as.matrix(innertmp2)
          innertmp2[is.na(innertmp2)] <- 0
          dim(innertmp2)
          
          innertrainingdata <- as.kernelMatrix(innertmp1)
          innertestingdata <- as.kernelMatrix(innertmp2)
          
          
          #print(costcounter[i])
          svm.model_tune <- kernlab::ksvm(y=as.factor(innerytr),
                                          x=innertrainingdata,
                                          kernel='matrix',
                                          C=costcounter[i]
          )
          summary(svm.model_tune)
          #dim(innertestingdata)
          y.innersvm[innertestIndex] <- kernlab::predict(object=svm.model_tune, 
                                                         newdata=innertestingdata, 
                                                         type="response")
          length(y.innersvm)
        }
        if(innerfold==10){
          y.innersvm = y.innersvm -1
          library(AUC)  
          innercvAUC=auc(roc(y.innersvm, as.factor(Xtrain$sideeffect)))
          cat("\nEvaluation on one inner Fold dataset:\n\n")
          print(innercvAUC)
          inner_test_accuracy_matrix[i,] <- innercvAUC
        }
        
        
      }
      
      optimalCost <- which(inner_test_accuracy_matrix %in% max(inner_test_accuracy_matrix))
      
      Xtrain = Xtrain[,-which(colSums(dataset[trainingIndex,])==0)]
      dim(Xtrain)
      #Xtest = dataset[testIndex,]
      Xtest = Xtest[,-which(colSums(dataset[trainingIndex,])==0)]
      dim(Xtest)
      
      
      ############# TUNNING PARAMETERS    
      xtr <- subset(Xtrain, select=-sideeffect)
      #ytr <- as.factor(Xtrain$sideeffect)
      ytr <- Xtrain$sideeffect
      xtr = na.omit(xtr)
      ytr = na.omit(ytr)
      
      xte <- subset(Xtest, select=-sideeffect)
      #yte <- as.factor(Xtest$sideeffect)
      yte <- Xtest$sideeffect
      xte = na.omit(xte)
      yte = na.omit(yte)
      
      

      set.seed(randomIterations[r])
      ## S4 method for signature 'matrix'
      
      tmp1 <- jaccard(t(as.matrix(xtr)), t(as.matrix(xtr)))
      tmp1 <- as.matrix(tmp1)
      tmp1[is.na(tmp1)] <- 0
      dim(tmp1)
      tmp2 <- jaccard(t(as.matrix(xte)), t(as.matrix(xtr)))
      tmp2 <- as.matrix(tmp2)
      tmp2[is.na(tmp2)] <- 0
      dim(tmp2)
      
      trainingdata <- as.kernelMatrix(tmp1)
      testingdata <- as.kernelMatrix(tmp2)
      
      
      ytr = as.matrix(ytr)
      class(ytr)='numeric'
      yte = as.matrix(yte)
      class(yte)='numeric'
      class(ytr)
      
      #costGridRange= list(cost = 2^(2:4))
      
      
      if(optimalCost==1){costRange = 1}
      if(optimalCost==2){costRange = 5}
      if(optimalCost==3){costRange = 10}
      
      cat("\nOptimal Cost selected:\n\n")
      print(optimalCost)
      
      svm.model_after_tune <- kernlab::ksvm(y=as.factor(ytr),
                                      x=trainingdata,
                                      kernel='matrix',
                                      C=costRange
                                      #,cross=10
      )
      summary(svm.model_after_tune)
      dim(testingdata)
      
      y.svm[testIndex] <- kernlab::predict(object=svm.model_after_tune, 
                                           newdata=testingdata, 
                                           type="response")
      Xtest$sideeffect
      datasety
      #levels(Xtest$sideeffect)
      
      if(fold==10) {
        y.svm = y.svm -1
        str(y.svm)
        #y.svm <- as.factor(y.svm)
        
        library(AUC)  
        cvAUC=auc(roc(y.svm, as.factor(datasety)))
        #print(cvAUC)
        
        cat("\n One Fold Data set Confusion matrix:\n")
        confusion_matrix <- table(pred = y.svm, true = datasety);
        confusion_matrix
        
        
        cat("\nEvaluation on Fold dataset:\n\n")
        accuracy = cvAUC
        print(accuracy)
  #      accuracy = sum(datasety == y.svm)/length(y.svm)
        precision = confusion_matrix[1,1]/sum(confusion_matrix[,1])
        recall = confusion_matrix[1,1]/sum(confusion_matrix[1,])
        f = 2 * (precision * recall) / (precision + recall)
        cat(paste("Accuracy:\t", format(accuracy, digits=2), "\n",sep=" "))
        cat(paste("Precision:\t", format(precision, digits=2), "\n",sep=" "))
        cat(paste("Recall:\t\t", format(recall, digits=2), "\n",sep=" "))
        cat(paste("F-measure:\t", format(f, digits=2), "\n",sep=" "))
        
        accuracy_matrix_full[chunks,fold] <- accuracy
        #accuracy_matrix_full[chunks,fold] <- sum(datasety == y.svm)/length(y.svm)
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



a <- rowMeans(t(am))
p <- rowMeans(t(pm))
r <- rowMeans(t(rm))
f <- rowMeans(t(fm))


svmJ <- cbind(a,p,r,f)
sd(svmJ[,1])
sd(svmJ[,2])
sd(svmJ[,3])
sd(svmJ[,4])

mean(svmJ[,1])
mean(svmJ[,2])
mean(svmJ[,3])
mean(svmJ[,4])

save(svmJ, file = "svmJ_DI_Headache.RData")


svmLin <- cbind(a,p,r,f)
sd(svmLin[,1])
sd(svmLin[,2])
sd(svmLin[,3])
sd(svmLin[,4])

mean(svmLin[,1])
mean(svmLin[,2])
mean(svmLin[,3])
mean(svmLin[,4])

boxplot(svmLin[,1], main="SVM Linear Kernel Accuracy")
boxplot(svmLin[,2], main="SVM Linear Kernel Precision")
boxplot(svmLin[,3], main="SVM Linear Kernel Recall")
boxplot(svmLin[,4], main="SVM Linear Kernel F-measure")

#save(svmLin, file = "svmLinDIFP.RData")


am_svmrbf 
pm_svmrbf
rm_svmrbf 
fm_svmrbf


a <- rowMeans(t(am_svmrbf))
p <- rowMeans(t(pm_svmrbf))
r <- rowMeans(t(rm_svmrbf))
f <- rowMeans(t(fm_svmrbf))


svmRbf <- cbind(a,p,r,f)
sd(svmRbf[,1])
sd(svmRbf[,2])
sd(svmRbf[,3])
sd(svmRbf[,4])

mean(svmRbf[,1])
mean(svmRbf[,2])
mean(svmRbf[,3])
mean(svmRbf[,4])

boxplot(svmRbf[,1], main="SVM Gaussian Kernel Accuracy")
boxplot(svmRbf[,2], main="SVM Gaussian Kernel Precision")
boxplot(svmRbf[,3], main="SVM Gaussian Kernel Recall")
boxplot(svmRbf[,4], main="SVM Gaussian Kernel F-measure")

svmRbf
#save(svmRbf, file = "svmRbfDIFP.RData")


######################
http://r.789695.n4.nabble.com/SVM-cross-validation-in-e1071-td896822.html
http://kyrcha.info/ml-tutorials/svm-in-R.html
svm on disease diagnostics
https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4264614/
  
  load('randomClassifierwithoutprob.RData')
randomClassifierwithoutprob <- rrrrrrr
mean(randomClassifierwithoutprob[,1])
mean(randomClassifierwithoutprob[,2])
mean(randomClassifierwithoutprob[,3])
mean(randomClassifierwithoutprob[,4])

sd(rrrrrrr[,1])
sd(rrrrrrr[,2])
sd(rrrrrrr[,3])
sd(rrrrrrr[,4])


load('nB.RData')  
mean(nB[,1])
mean(nB[,2])
mean(nB[,3])
mean(nB[,4])

sd(nB[,1])
sd(nB[,2])
sd(nB[,3])
sd(nB[,4])

load('randomClassifier.RData')
mean(rrrrrrr[,1])
mean(rrrrrrr[,2])
mean(rrrrrrr[,3])
mean(rrrrrrr[,4])

sd(rrrrrrr[,1])
sd(rrrrrrr[,2])
sd(rrrrrrr[,3])
sd(rrrrrrr[,4])

load('svmRbf.RData')  
svmRbf
mean(svmRbf[,1])
mean(svmRbf[,2])
mean(svmRbf[,3])
mean(svmRbf[,4])
load('svmLin.RData')
svmLin
mean(svmLin[,1])
mean(svmLin[,2])
mean(svmLin[,3])
mean(svmLin[,4])
load('lda.RData')
mean(ldaLin[,1])
mean(ldaLin[,2])
mean(ldaLin[,3])
mean(ldaLin[,4])

sd(ldaLin[,1])
sd(ldaLin[,2])
sd(ldaLin[,3])
sd(ldaLin[,4])


load('nn_2.RData')
mean(nn[,1])
mean(nn[,2])
mean(nn[,3])
mean(nn[,4])

sd(nn[,1])
sd(nn[,2])
sd(nn[,3])
sd(nn[,4])



load('randomForest.RData')
mean(rf[,1])
mean(rf[,2])
mean(rf[,3])
mean(rf[,4])

sd(rf[,1])
sd(rf[,2])
sd(rf[,3])
sd(rf[,4])
load('lr_glmnet_lasso.RData')  
lrrr
lr
mean(lr[,1])



bplt <-boxplot(rf[,1],nn[,1],lr[,1],
               ldaLin[,1],svmLin[,1],svmRbf[,1],
               randomClassifierwithoutprob[,1],nB[,1],randomClassifier[,1], main="Accuracy Measure", 
               xlab="Prediction Performance of Classifiers", col=c2)

text(x= 1:9, y= 0.41, labels= c("RF","NN","LR","LDA", "SL", "SR", "B", "NB", "B" ), col=c2)



# RANDOM FOREST T-TEST

t.test(rf[,1], nn[,1],alternative="greater", paired=TRUE) #0.01215834
t.test(rf[,1], ldaLin[,1],alternative="greater", paired=TRUE) #0.00003563147
t.test(rf[,1], svmLin[,1],alternative="greater", paired=TRUE) #0.000004755216
t.test(rf[,1], svmRbf[,1],alternative="greater", paired=TRUE) #0.00002380048
t.test(rf[,1], lr[,1],alternative="greater", paired=TRUE) #0.005185662
load('randomClassifierwithoutprob.RData')
t.test(rf[,1], rrrrrrr[,1],alternative="greater", paired=TRUE) #0.000006233546
load('randomClassifier.RData')
t.test(rf[,1], rrrrrrr[,1],alternative="greater", paired=TRUE) #0.0003286356
t.test(rf[,1], nB[,1],alternative="greater", paired=TRUE) #0.00000000003131495

?p.adjust()

# NEURAL NETWORK T-TEST
mean(lr[,1])
t.test(nn[,1], rf[,1],alternative="greater", paired=TRUE) #0.9878417
t.test(nn[,1], ldaLin[,1],alternative="greater", paired=TRUE) #0.0007309081
t.test(nn[,1], svmLin[,1],alternative="greater", paired=TRUE) #0.000008583346
t.test(nn[,1], svmRbf[,1],alternative="greater", paired=TRUE) #0.0005706092
t.test(nn[,1], lr[,1],alternative="greater", paired=TRUE) #0.4146337
load('randomClassifierwithoutprob.RData')
t.test(nn[,1], rrrrrrr[,1],alternative="greater", paired=TRUE) #0.00003883929
load('randomClassifier.RData')
t.test(nn[,1], rrrrrrr[,1],alternative="greater", paired=TRUE) #0.0004422565
t.test(nn[,1], nB[,1],alternative="greater", paired=TRUE) #0.00000000005389967


# LOGISTIC REGRESSION T-TEST
mean(nn[,1]) mean(lr[,1])
t.test(lr[,1], rf[,1],alternative="greater", paired=TRUE) #0.9948143
t.test(lr[,1], ldaLin[,1],alternative="greater", paired=TRUE) #0.00006517283
t.test(lr[,1], svmLin[,1],alternative="greater", paired=TRUE) #0.000002383529
t.test(lr[,1], svmRbf[,1],alternative="greater", paired=TRUE) #0.00292595
t.test(lr[,1], nn[,1],alternative="greater", paired=TRUE) #0.5853663
load('randomClassifierwithoutprob.RData')
t.test(lr[,1], rrrrrrr[,1],alternative="greater", paired=TRUE) #0.00002534547
load('randomClassifier.RData')
t.test(lr[,1], rrrrrrr[,1],alternative="greater", paired=TRUE) #0.0006985429
t.test(lr[,1], nB[,1],alternative="greater", paired=TRUE) #0.00000000002231559


# LINEAR DISCRIMINANT ANALYSIS T-TEST

t.test(ldaLin[,1], rf[,1],alternative="greater", paired=TRUE) #0.9999644
t.test(ldaLin[,1], lr[,1],alternative="greater", paired=TRUE) #0.9999348
t.test(ldaLin[,1], svmLin[,1],alternative="greater", paired=TRUE) #0.2099581
t.test(ldaLin[,1], svmRbf[,1],alternative="greater", paired=TRUE) #0.7597287
t.test(ldaLin[,1], nn[,1],alternative="greater", paired=TRUE) #0.9992691
load('randomClassifierwithoutprob.RData')
t.test(ldaLin[,1], rrrrrrr[,1],alternative="greater", paired=TRUE) #0.0001202768
load('randomClassifier.RData')
t.test(ldaLin[,1], rrrrrrr[,1],alternative="greater", paired=TRUE) #0.00504318
t.test(ldaLin[,1], nB[,1],alternative="greater", paired=TRUE) #0.0000000001123456



# SUPPORT VECTOR MACHINES - LINEAR T-TEST

t.test(svmLin[,1], rf[,1],alternative="greater", paired=TRUE) #0.9999952
t.test(svmLin[,1], lr[,1],alternative="greater", paired=TRUE) #0.9999976
t.test(svmLin[,1], ldaLin[,1],alternative="greater", paired=TRUE) #0.7900419
t.test(svmLin[,1], svmRbf[,1],alternative="greater", paired=TRUE) #0.8993841
t.test(svmLin[,1], nn[,1],alternative="greater", paired=TRUE) #0.9999914
load('randomClassifierwithoutprob.RData')
t.test(svmLin[,1], rrrrrrr[,1],alternative="greater", paired=TRUE) #0.0002883686
load('randomClassifier.RData')
t.test(svmLin[,1], rrrrrrr[,1],alternative="greater", paired=TRUE) #0.003862401
t.test(svmLin[,1], nB[,1],alternative="greater", paired=TRUE) #0.00000000005206811




# SUPPORT VECTOR MACHINES - RADIAL T-TEST

t.test(svmRbf[,1], rf[,1],alternative="greater", paired=TRUE) #0.9999762
t.test(svmRbf[,1], lr[,1],alternative="greater", paired=TRUE) #0.997074
t.test(svmRbf[,1], ldaLin[,1],alternative="greater", paired=TRUE) #0.2402713
t.test(svmRbf[,1], svmLin[,1],alternative="greater", paired=TRUE) #0.1006159
t.test(svmRbf[,1], nn[,1],alternative="greater", paired=TRUE) #0.9994294
load('randomClassifierwithoutprob.RData')
t.test(svmRbf[,1], rrrrrrr[,1],alternative="greater", paired=TRUE) #0.00006452977
load('randomClassifier.RData')
t.test(svmRbf[,1], rrrrrrr[,1],alternative="greater", paired=TRUE) #0.004896864
t.test(svmRbf[,1], nB[,1],alternative="greater", paired=TRUE) #0.0000000005926963




# Naive Beyes

t.test(nB[,1], rf[,1],alternative="greater", paired=TRUE) #1
t.test(nB[,1], lr[,1],alternative="greater", paired=TRUE) #1
t.test(nB[,1], ldaLin[,1],alternative="greater", paired=TRUE) #1
t.test(nB[,1], svmLin[,1],alternative="greater", paired=TRUE) #1
t.test(nB[,1], nn[,1],alternative="greater", paired=TRUE) #1
load('randomClassifierwithoutprob.RData')
t.test(nB[,1], rrrrrrr[,1],alternative="greater", paired=TRUE) #0.3211951
load('randomClassifier.RData')
t.test(nB[,1], rrrrrrr[,1],alternative="greater", paired=TRUE) #0.9675895
t.test(nB[,1], svmRbf[,1],alternative="greater", paired=TRUE) #1



# BASELINE

load('randomClassifierwithoutprob.RData')
randomClassifierwithoutprob <- rrrrrrr
t.test(randomClassifierwithoutprob[,1], nB[,1],alternative="greater", paired=TRUE) #0.6788049
t.test(randomClassifierwithoutprob[,1], rf[,1],alternative="greater", paired=TRUE) #0.9999938
t.test(randomClassifierwithoutprob[,1], lr[,1],alternative="greater", paired=TRUE) #0.9999747
t.test(randomClassifierwithoutprob[,1], ldaLin[,1],alternative="greater", paired=TRUE) #0.9998797
t.test(randomClassifierwithoutprob[,1], svmLin[,1],alternative="greater", paired=TRUE) #0.9997116
t.test(randomClassifierwithoutprob[,1], nn[,1],alternative="greater", paired=TRUE) #0.9999612
load('randomClassifier.RData')
t.test(randomClassifierwithoutprob[,1], rrrrrrr[,1],alternative="greater", paired=TRUE) #0.8864178
t.test(randomClassifierwithoutprob[,1], svmRbf[,1],alternative="greater", paired=TRUE) #0.9999355


# BASELINE 50:50
load('randomClassifier.RData')
randomClassifier <- rrrrrrr
load('randomClassifierwithoutprob.RData')
randomClassifierwithoutprob <- rrrrrrr
t.test(randomClassifier[,1], nB[,1],alternative="greater", paired=TRUE) #0.03241
t.test(randomClassifier[,1], rf[,1],alternative="greater", paired=TRUE) #0.9997
t.test(randomClassifier[,1], lr[,1],alternative="greater", paired=TRUE) #0.9911
t.test(randomClassifier[,1], ldaLin[,1],alternative="greater", paired=TRUE) #0.995
t.test(randomClassifier[,1], svmLin[,1],alternative="greater", paired=TRUE) #0.9961
t.test(randomClassifier[,1], nn[,1],alternative="greater", paired=TRUE) #0.9995577
t.test(randomClassifier[,1], randomClassifierwithoutprob[,1],alternative="greater", paired=TRUE) #0.1136
t.test(randomClassifierwithoutprob[,1], svmRbf[,1],alternative="greater", paired=TRUE) #0.9999



load('randomClassifier.RData')
randomClassifier <- rrrrrrr
load('randomClassifierwithoutprobabilities.RData')
randomClassifierwithoutprob <- rrrrrrr
load('randomForest.RData')
load('lr_glmnet_lasso.RData')  
load('nB.RData')  
load('nn_2.RData')
load('lda.RData')
load('svmRbf.RData')  
load('svmLin.RData')  

bplt <-boxplot(rf[,1],nn[,1],lr[,1],
               ldaLin[,1],svmLin[,1],svmRbf[,1],
               randomClassifierwithoutprob[,1],nB[,1],randomClassifier[,1], main="Accuracy Measure for Disease Indications", 
               xlab="Prediction Performance of Classifiers", col=c2)

text(x= 1:9, y= 0.41, labels= c("RF","NN","LR","LDA", "SL", "SR", "B", "NB", "B" ), col=c2)


