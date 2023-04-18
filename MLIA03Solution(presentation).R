# poll ev questions https://www.polleverywhere.com/activities 
##############################################
################ Solution1.r #################
##############################################  

###### IA03 Q01: FOR LOOP PRACTICE #####
rm(list=ls())
my.matrix <- matrix(rep(0,3*4), nrow=3, ncol=4)  # 3 x 4 matrix, my.matrix, filled with zeros 
running.total <- 0

for (i in 1: 3) {
  for (j in 1: 4) { 
    my.matrix[i,j] <- i + j # populate each element (i, j) with the value of (i + j)
    running.total <- running.total + my.matrix[i,j]
    print(paste(my.matrix[i,j] ," " , running.total))
      # PRINT: current value (i + j), and running total of values added (12 print outs)
  }
}


##############################################
################ Solution2.r #################
##############################################

rm(list=ls())
set.seed(5082)

###### IA03 Q02: PREPARE DATAFRAME ######
zillow = read.csv("zillow2223.csv", header=TRUE, sep=',', row.names="name", stringsAsFactors = TRUE) # read in data
zillow$zipcode <- factor(zillow$zipcode) # Make zipcode factor, so model runs correctly
zillow$age <- 2023 - zillow$year # Calculate the age of the house variable ("age") and add to dataframe
zillow<-subset(zillow, select=-c(year)) # Remove feature "year". No longer needed since we have age
attach(zillow)

### CORRECTED CODE
n = nrow(zillow)

# split the train (70%) and test (30%) sets
trainIndex = sample(1:n, size = n*0.7)  # WAS: trainIndex = sample(1:n, size = 0.8)
length(trainIndex) #Q02-1

# Creating train.x, test.x, train.y, and test.y. 
# train.x and test.x as subsets of the entire data frame, and Y target = zipcode.

train.x = zillow[trainIndex,] # WAS: train.x = x[-trainIndex] 
dim(train.x) #Q02-2

test.x = zillow[-trainIndex,] # WAS: test.x = x[trainIndex ,] 
dim(test.x) #Q02-3

train.y = zillow$zipcode[trainIndex] # WAS: # train.y = y[trainIndex,] 
summary(train.y) #Q02-4

test.y = zillow$zipcode[-trainIndex] # WAS: test.y = y[trainIndex] 
length(test.y) #Q02-5

### WAS: ###
# n = nrow(zillow)

# train.x = x[-trainIndex] (train.x composed of testIndex, subsetting non-existent dataframe, x, that should be 2 dimentional feature matrix (comma))
# test.x = x[trainIndex,] (test.x composed of trainIndex, subsetting non-existent dataframe, x)
# train.y = y[trainIndex,] (train.y subsetting non-existent dataframe, y, that should be Y target vector (remove comma))
# test.y = y[trainIndex] (test.y composed of trainIndex, subsetting non-existent dataframe, y)


####### IA03 Q03: Random Forest ######
library(randomForest)
set.seed(5082)

### CORRECTED CODE ###
## (NOTE: Y target has changed since Q02) ##
rf.zillow <- randomForest(zillow$willingtopay~. -price -zestimate, # Predict "willingtopay", excluding "price" and "zestimate".
                          data=zillow,
                          subset=trainIndex, #  using train set index vector (created in Q02)
                          mtry=5, # 5 features randomly sampled as candidates at each split.
                          importance = TRUE) # and the importance parameter to TRUE.
# calculating train RMSE
yhat<-predict(rf.zillow,
              newdata=train.x)
sqrt(mean((yhat-zillow$willingtopay[trainIndex])^2)) #Q03-1

### WAS ###
# rf.zillow <- randomForest(zillow$willingtopay~. -price -zestimate, 
#                            data=zillow, 
#                            subset=train.x, (subset expects subset index vector)
#                            mtry=6, (should be 5)
#                            importance = TRUE)
# yhat<-predict(rf.zillow, 
#               newdata=train.y) (newdata should be train.x or zillow[trainIndex,])
# mean((yhat-trainIndex)^2) (This is MSE, not RMSE. Should be subtracting train data for y target)

varImpPlot(rf.zillow) # variable importance plot 
                      # Which feature has the highest importance?
                      # removing ____ from model would result in additional misclassification of approx. 
                      # 24 observations on average


####### IA03 Q04 Tune Random Forest ######
set.seed(5082)
# Print the content of the training RMSE vector. (#Q04-1)
#   Print the number of features that produces the best training RMSE using either which.max or which.min. (#Q04-2)
#Insert your code below
rmse<-rep(0,5) # Creating a 5-element zero vector to store RMSE values from the 5 tuned models\

for (i in 1:5){ # Train 5 random forest models,  
  rf.zillow <- randomForest(willingtopay~. -price -zestimate,
                            data=zillow,
                            subset=trainIndex,
                            mtry=i,  # tuning mtry to randomly sample 1 to 5 features as candidates at each split
                            importance = TRUE)
  
  yhat<-predict(rf.zillow,
                newdata=train.x)
  rmse[i]<-sqrt(mean((yhat-zillow$willingtopay[trainIndex])^2)) # Saving training RMSE values.
}

rmse #Q04-1
# Num features that produces the best training RMSE
which.min(rmse) #Q04-2


####### IA03 Q05 Tune Boosting #######
library(gbm)
set.seed(5082)

### CORRECTED CODE 
mse.boost<-rep(0,4*5) # vector for storing training MSE values from models tuned
counter <-0
for (i in 1:4) {
  for (j in 1:5) {
    counter <- counter +1 # counter for indexing mse vector
    # Boosted regression tree models
    boost.zillow<-gbm(zestimate~.-price -willingtopay, # predict zestimate, excluding price and willingtopay
                      data=zillow[trainIndex,], # using the training set
                      distribution= "gaussian", # numeric prediction, not classification
                      n.trees = 5000, # 5000 trees
                      interaction.depth= i, # varying tree depths of 1- 4 (splits per tree)
                      shrinkage = 0.01*j, #learning rate = 0.01 to 0.05
                      verbose = F)
    
    # Compute training MSE 
    yhat<-predict(boost.zillow,
                  newdata=zillow[trainIndex,],
                  n.trees=5000)
    mse.boost[counter]<-mean((yhat-zillow$zestimate[trainIndex])^2) #Q04-3
  }
}
mse.boost #Q05-1

### WAS ###
# rmse<-rep(0,4) (vector is too small, why is it called rmse?!?)
# for i in 1:4 { (only tunes interaction.depth)
#   boost.zillow<-gbm(zestimate~.-price -willingtopay, 
#                     data=zillow[trainIndex,], 
#                     distribution= "bernoulli", # (this is not classification)
#                     n.trees = 5000, 
#                     interaction.depth= [i], 
#                     shrinkage = j, # (j is non-existant iterator)
#                     verbose = F) 
# 
#   yhat<-predict(boost.zillow, 
#                 newdata=zillow[-trainIndex], # (TRAIN MSE, not test)
#                 n.trees=5000)
#   rmse[i]<-mean((yhat-zillow[i])^2) # (looking at each record of zillow data?!?)
# }

###### IA03 Q06: Tune SVM 1 ###### 
library(e1071)
set.seed(5082)

#Fix the code below
# tune.out <- tune(svm.zillow,
#                  train.x, 
#                  train.y,
#                  data=zillow,
#                  kernal ="linear",
#                  ranges=list(cost=c(0.1, 0.5, 1)))
# confusion<-table(test.y, tune.out)
# confusion
# confusion[1] + confusion[4]/sum(confusion)

#  using the tune() function with the following specs:
#   i. 
# ii. 
# iii. all other variables as X features
# iv. 
# v. 3 
# Print your tune() output from step 1. (#Q06-1)
#   Save the confusion matrix of the predicted values versus the actual values (in that order) using the best model from tune() into an object named confusion.
#   Print the content of confusion. (#Q06-2)
#     Update the starter code to print the model’s accuracy rate using elements of confusion. (#Q06-3)

### CORRECTED CODE
# Tune SVM models
tune.out <- tune(svm,
                 zipcode~., # zipcode as the Y target
                 data=zillow, # using zillow data frame
                 kernel ="linear", # the linear kernel
                 ranges=list(cost=c(0.1, 0.5, 1))) # cost of misclassification
tune.out #Q06-1

yhat<-predict(tune.out$best.model, data=zillow)
confusion<-table(yhat, zillow$zipcode) # predicted values versus the actual values confusion matrix
confusion #Q06-2
(confusion[1] + confusion[4])/sum(confusion) #Q06-3 model’s accuracy rate



####### IA03 Q07: Tune SVM 2 ######
set.seed(5082)

#Fix the code below
# tune.out<-tune(svm,
#             zillow$zipcode~.,
#             data=train.x,
#             kernel="radial",
#             ranges = list(c(cost=c(.01,.1, 5),
#                             gamma=c(.001, .01, 5)))
#               )
# tune.out
# summary(tune.out)


### CORRECTED CODE
tune.out<-tune(svm,
               zipcode~., # zipcode as the Y target
               data=train.x, # training set
               kernel="radial", # radial kernel
               ranges = list(cost=c(.01,.1, 5), # cost of misclassification
                             gamma=c(.001, .01, 5)) # curvature
)
summary(tune.out) #Q07-1
# (higher error)

##### Solution3.r******
rm(list=ls())
set.seed(5082)

####### IA03 Q08: PCA #######
zillow = read.csv("zillow2223.csv", header=TRUE, sep=',', row.names="name", stringsAsFactors = TRUE)
zillow$age <- 2022 - zillow$year # # Calculate the age of the house variable ("age") and add to dataframe
# Removing features 
zillow<-subset(zillow, select=-c(year, zipcode, zestimate, willingtopay, price, type, international, section, team)) 
attach(zillow)

#Insert your solution code below
pr.out <-prcomp(zillow, 
                scale=T)   # PCA with scaled zillow data frame
pr.out$rotation[,"PC1"] #Q8-1: PC1 loadings (eiganVectors)
pr.out$x["SethL", "PC2"] #Q08-2: P2 score matrix
biplot(pr.out, scale=0) 
pr.var<-pr.out$sdev^2 
pr.var #Q08-3: Variance Explained (how much variation can be attributed to each of the principal components)

sum(pr.var) #Q08-4: Total Variance Explained

pve<-pr.var/sum(pr.var)
pve #Q8-5: Proportion of Variance Explained

cve<-cumsum(pve)
cve #Q8-6: Cumulative Variance Explained

####### IA03 Q09: K-Means ####### 
km.out <- kmeans(zillow, 3, nstart = 20) # 3 clusters, 20 re-runs (different initial configs, to avoid local optimum)
km.out$cluster #Q09-1 cluster assignments of each record
