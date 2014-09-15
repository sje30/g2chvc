#Functions for classifier_final.R

###DECISION TREE FUNCTIONS###

#Function to calculate accuracy and important features from boosted trees
#Classifies into 20 categories of combined region and DIV
#Input is data.df, output is prediction accuracy and average importance of each feature,
#based on its Mean Decrease in Gini
boost.tree.all<-function(data.df) {
  ret<-NULL
  classes<-paste(data.df[,1], data.df[,2])
  class.df<-cbind(classes, data.df[,-1*c(1,2)])
  n.row<-nrow(class.df)
  #Set size of training set to 2/3 of total data
  Ntrain<-round(n.row*2/3)
  correct<-0
  imp<-rep(0,11)
  #Perform 500 trials with different training sets
  for (i in 1:500) {
    traindat<-sample(1:nrow(class.df), Ntrain)
    testdat<-class.df[-traindat ,]
    #Calculate boosted classification tree, using all 11 features and 500 trees
    tree.out<-randomForest(classes~. ,data=class.df[traindat,], mtry=11,importance =TRUE, ntree=500)
    tree.pred<-predict(tree.out,testdat,type="class")
    correct<- sum(tree.pred==testdat$classes)/(n.row-Ntrain)+ correct
    imp<-importance(tree.out)[,"MeanDecreaseGini"]+imp
  }
  avg.imp<-imp/500
  ret$accuracy<-correct/500
  ret$factors<-avg.imp
  ret
}

#Function to calculate accuracy and important factors from boosted tree for each region
#Imput is data.df and region ("ctx" or "hpc"), output is prediction accuracy and average importance of each feature
boost.tree.region<-function(data.df, region1, nreps) {
  ret<-NULL
  reg.set<-subset(data.df,region==region1)[,-1]
  n.row<-nrow(reg.set)
  #Set size of training set to 2/3 of total data
  Ntrain<-round(n.row*2/3)
  correct<-0
  imp<-rep(0,11)
  #Perform NREPS trials with different training sets 
  for (i in 1:nreps) {
    traindat<-sample(1:nrow(reg.set), Ntrain)
    testdat<-reg.set[-traindat ,]
    #Calculate boosted classification tree, using all 11 features and NREPS trees
    tree.out<-randomForest(age~. ,data=reg.set[traindat, ],
                           mtry=11,importance =TRUE, ntree=nreps)
    tree.pred<-predict(tree.out,testdat,type="class")
    #Round predicted age to closest age from ages vector
    rounded.pred<-sapply(tree.pred, round.fn)
    correct<- sum(rounded.pred==testdat$age)/(n.row-Ntrain)+ correct
    imp<-importance(tree.out)[,"%IncMSE"]+imp
  }
  avg.imp<-imp/nreps
  ret$accuracy<-correct/nreps
  ret$factors<-avg.imp
  ret
}

#Function that rounds input number to closest age in ages vector
round.fn<-function(y){
  ages[which.min(abs(ages-y))]
}

#Function to calculate accuracy and important factors from boosted tree for each age DIV
#Imput is data.df and region ("ctx" or "hpc"), output is prediction accuracy and average importance of each feature,
#based on its Mean Decrease in Gini
boost.tree.age<-function(data.df, age1, nreps=500) {
  ret<-NULL
  age.set<-subset(data.df,age==age1)[,-2]
  n.row<-nrow(age.set)
  #Set size of training set to 2/3 of total data
  Ntrain<-round(n.row*2/3)
  correct<-0
  imp<-rep(0,11)
  #Perform NREPS trials with different training sets 
  for (i in 1:nreps) {
    traindat<-sample(1:nrow(age.set), Ntrain)
    testdat<-age.set[-traindat ,]
    #Calculate boosted classification tree, using all 11 features and nreps trees
    tree.out<-randomForest(region~. ,data=age.set[traindat, ], mtry=11,importance =TRUE, ntree=nreps)
    tree.pred<-predict(tree.out,testdat,type="class")
    correct<- sum(tree.pred==testdat$region)/(n.row-Ntrain)+ correct
    imp<-importance(tree.out)[,"MeanDecreaseGini"]+imp
  }
  avg.imp<-imp/nreps
  ret$accuracy<-correct/nreps
  ret$factors<-avg.imp
  ret
}

##SUPPORT VECTOR MACHINE FUNCTIONS###

#Function to calculate accuracy from SVM model at one DIV
#Input is data, age, kernel type (linear, polynomial, radial), test type ("test.set" which uses 2/3 of data
#as trainind set or "LOOCV" for leave one out cross validation), and optional vector of features to exclude
#Output is average prediction accuracy over all trials
svm.calc<-function(data.df, age1, kernel, test.type, rm.fac=NULL) {
  age.set<-subset(data.df,age==age1)
  n<-sum(age.set$region=="ctx")
  #Convert region to 0 for cortex and 1 from hippocampus
  bin.region<-c(rep(0,n), rep(1,length(age.set$region)-n))
  age.set<-cbind(as.factor(bin.region), age.set[3:13])
  names(age.set)[1]<-"bin.region"
  n.row<-nrow(age.set)
  accuracy<-NULL
  res.df<-NULL
  #Remove features included in the rm.fac vector
  if (!is.null(rm.fac)) {
    age.set<-age.set[,-1*rm.fac]
  }
  #Run SVM on all data to determine the optimal model
  if (kernel=="linear") {
    tune.out<-tune(svm, bin.region~. , data=age.set, kernel="linear", ranges = list(cost=c(0.001,0.01,0.1,1,5,10,100)) )
  } else if (kernel == "poly") {
    tune.out<-tune(svm, bin.region~. , data=age.set, kernel="polynomial", degree=2, ranges = list(cost=c(0.001,0.01,0.1,1,5,10,100)) ) 
  } else if (kernel == "radial") {
    tune.out<-tune(svm, bin.region~. , data=age.set, kernel="radial", gamma=(1/11), ranges = list(cost=c(0.001,0.01,0.1,1,5,10,100) ) )
  }
  #Set cost to optimal cost from full model
  bestmod<-tune.out$best.model
  cst<-bestmod$cost
  #Test model by using train.frac proportion of data as training set, and remaining as test set
  if (test.type=="test.set") {
    #Set training set to be 2/3 of all data
    Ntrain<-round(n.row*2/3)
    #Determing accuracy from 100 trials with random test sets
    for (i in 1:100) {
      trainnum<-sample(1:nrow(age.set), Ntrain)
      traindat<-age.set[trainnum, ]
      testdat<-age.set[-trainnum ,]
      accuracy[i]<-svm.test(traindat, testdat, kernel, cst)
    }
  } else if (test.type=="LOOCV")  {     #Test using leave one out cross validation
    for (i in 1:n.row) {
      testdat<-age.set[i,]
      traindat<-age.set[-i,]
      accuracy[i]<-svm.test(traindat, testdat, kernel, cst)
    }
  }
  mean(accuracy)
}

#Function to calculate the prediction accuracy of an SVM. Input is training data, test data,
#kernel (linear, polynomial or radial) and cost. Output is proportion of accuracy predictions.
svm.test<-function(traindat, testdat, kernel, cst) {
  if (kernel=="radial") {
    svmfit<-svm(bin.region~. , data=traindat, kernel="radial",gamma=(1/11), cost=cst)
  } else if (kernel=="linear") {
    svmfit<-svm(bin.region~. , data=traindat, kernel="linear", cost=cst)
  } else if (kernel=="polynomial") {
    svmfit<-svm(bin.region~. , data=traindat, kernel="polynomial", degree=2, cost=cst)
  }
#Measure accuracy as proportion of test set that are correctly predicted
  test.pred<-predict(svmfit,testdat)
  accuracy<- sum(test.pred==testdat$bin.region)/length(test.pred)
}

#Calculate prediction accuracy of model with num.rm features removed. 
#Input is data, matrix of ordered important features for each DIV, number of features to be removed,
#type of kernel ("linear", "radial" or "polynomial") and test.type ("test.set" or "LOOCV")
#Output is prediction accuracy for each DIV. 
feature.rm<-function(data.df, order.mat, num.rm, kernel, test.type) { 
  acc<-NULL
  for (j in 1:10) {
    acc[j]<-svm.calc(data.df, ages[j], kernel, test.type, (1 +order.mat[1:num.rm,j]))
  }
  acc
}

avg.feature.rm<-function(data.df, avg.order, num.rm, kernel, test.type) { 
  acc<-sapply(ages, function(x) svm.calc(data.df, x, kernel, test.type, (1 +avg.order[1:num.rm])))
}

