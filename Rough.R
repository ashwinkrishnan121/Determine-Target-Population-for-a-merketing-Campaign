library(dplyr)
library(rpart)
library(rpart.plot)
library(caret)
library(ranger)
library(pROC)
library(pdp)
library(ggplot2)
library(gbm)
library(xgboost)
library(adabag)
library(ROCR)
library(lattice)
library(randomForest)
library(recipes)

#Importing data and looking at the summary

Customer_data <- read.csv(file="DS_Tech_Review_Dataset (1).txt", sep="|")
summary(Customer_data)
Customer_data$target <- factor(Customer_data$target)


## Some sample data

## Remove columns with more than 50% NA
Customer_data_mod <-  Customer_data[, -which(colMeans(is.na(Customer_data)) > 0.5)]
summary(Customer_data_mod)

##Remove rows with NA values
Customer_data_mod2 <- Customer_data_mod[-which(rowMeans(is.na(Customer_data_mod)) > 0), ]

str(Customer_data_mod2)


#Variable correction

Customer_data_mod2$MAJOR_CREDIT_CARD_LIF <- ifelse(Customer_data_mod2$MAJOR_CREDIT_CARD_LIF == 'U', 1,0)
Customer_data_mod2$product <- ifelse(Customer_data_mod2$product == 'VIDEO/DATA/VOICE', 0,
                                         ifelse(Customer_data_mod2$product == 'VIDEO/DATA ', 1,
                                                ifelse(Customer_data_mod2$product == 'VIDEO/DATA/VOICE/HOME',2,
                                                       ifelse(Customer_data_mod2$product == 'VIDEO/DATA/HOME', 3,
                                                              ifelse(Customer_data_mod2$product == 'VIDEO/VOICE',4,
                                                                     ifelse(Customer_data_mod2$product == 'VIDEO ONLY',5,6
                                                                            ))))))


#Feature Engineering
# Remove features with zero variance
Customer_data_mod2 <- Customer_data_mod2[ - as.numeric(which(apply(Customer_data_mod2, 2, var) == 0))]

#Seperate Categorical and Continuous variables

discreteL <- function(x) length(unique(x)) < 10

Customer_data_cont <- Customer_data_mod2[ , !sapply(Customer_data_mod2, discreteL)] 
Customer_data_categorical <- Customer_data_mod2[ , sapply(Customer_data_mod2, discreteL)] 


# Identify near zero variance predictors on the continuous data set
remove_cols <- nearZeroVar(Customer_data_cont, names = TRUE, 
                           freqCut = 2, uniqueCut = 20)

# Get all column names from Customer_data_cont: all_cols
all_cols <- names(Customer_data_cont)

# Remove from data: Customer_data_cont2
Customer_data_cont2 <- Customer_data_cont[ , setdiff(all_cols, remove_cols)]

#principal components analysis on the continuous variables

pZ <- prcomp(Customer_data_cont2, tol = 0.1)
summary(pZ)

pZ$rotation

PC_data_frame <- (cbind(pZ$x[,1], pZ$x[,2]))

colnames(PC_data_frame) <- c("PC1","PC2")
#Combining the categorical data frome and the PC data frame

Customer_data_corrected <- cbind(Customer_data_categorical,PC_data_frame)
names <- colnames(Customer_data_corrected)

#Exploratory Analysis

#Based on Movie colour
Customer_data_corrected %>% group_by(product) %>% 
   ggplot(aes(target)) + geom_bar(stat = "count")



# Partition the original_data into train/test sets
set.seed(1535)
index <- sample(nrow(Customer_data_corrected),nrow(Customer_data_corrected)*0.70)
train = Customer_data_corrected[index,]
test = Customer_data_corrected[-index,]

#Oversampling the data

campaign_failure = Customer_data_corrected[Customer_data_corrected$target==0,]
campaign_sucess = Customer_data_corrected[Customer_data_corrected$target==1,]

new_data <- campaign_failure
for (i in 1:80)
{
new_data <- rbind(new_data,campaign_sucess, deparse.level = 0, make.row.names = TRUE,
      stringsAsFactors = default.stringsAsFactors())
}

colnames(new_data) <- names
#Ratio of 1 to zero
nrow(new_data[new_data$target==1,])/(nrow(new_data[new_data$target==0,])+nrow(new_data[new_data$target==1,]))

# Partition the new_data into train/test sets
set.seed(1535)
index <- sample(nrow(new_data),nrow(new_data)*0.70)
train_oversampled = new_data[index,]
test_oversampled = new_data[-index,]


#Make the best tree model

target_rpart <- rpart(
  target ~ ., data = train_oversampled, 
  control = list(cp = 0.00012), method = "class"
)

#Plotting the tree

plotcp(target_rpart)
prp(target_rpart, extra = 1)
                                                                                                                                                                            m
#Prediction on training data

pred.tree1 <- predict(target_rpart, test_oversampled, type="class", na.action = na.omit)
table(test_oversampled$target, pred.tree1, dnn=c("Truth","Predicted"))

#Prediction on test data
pred.test.tree <- predict(target_rpart, test, type="class")
table(test$target, pred.test.tree, dnn=c("Truth","Predicted"))


#Variable importance

target_rpart$variable.importance


#ROC Curve ,Gain Chart and K-S Chart for test data and AUC values 0.95

target.test.random = predict(target_rpart,test, type="prob")
pred = prediction(target.test.random[,2], test$target)
perf = performance(pred, "tpr", "fpr")
plot(perf, colorize=TRUE)

slot(performance(pred, "auc"), "y.values")[[1]]  #0.945

gain <- performance(pred, "tpr", "rpp")
plot(gain, main = "Gain Chart")

ks=max(attr(perf,'y.values')[[1]]-attr(perf,'x.values')[[1]])
plot(perf,main=paste0(' KS=',round(ks*100,1),'%'))
lines(x = c(0,1),y=c(0,1))
print(ks); #0.80


##Random Forest

#Calculate the number of trees required- Optimal of 300 trees chosen


target.rf <- randomForest(
  target ~ ., 
  data = train_oversampled, 
  ntree = 800,
  mtry = 7,
  importance = FALSE
)

#Prediction on training data

pred.tree1 <- predict(target.rf, test_oversampled, type="class", na.action = na.omit)
table(test_oversampled$target, pred.tree1, dnn=c("Truth","Predicted"))

#Prediction on test data
pred.test.tree <- predict(target.rf, test, type="class")
table(test$target, pred.test.tree, dnn=c("Truth","Predicted"))


#ROC Curve ,Gain Chart and K-S Chart for test data and AUC values 0.925

target.test.random = predict(target.rf,test, type="prob")
pred = prediction(target.test.random[,2], test$target)
perf = performance(pred, "tpr", "fpr")
plot(perf, colorize=TRUE)

slot(performance(pred, "auc"), "y.values")[[1]]  #0.926

gain <- performance(pred, "tpr", "rpp")
plot(gain, main = "Gain Chart")

ks=max(attr(perf,'y.values')[[1]]-attr(perf,'x.values')[[1]])
plot(perf,main=paste0(' KS=',round(ks*100,1),'%'))
lines(x = c(0,1),y=c(0,1))
print(ks);   #0.68

# Extract tibble of variable importance scores
vip::vi(target.rf)

# Construct ggplot2-based variable importance plot
vip::vip(target.rf, num_features = 10)

#Patial dependencies plot
partialPlot(target.rf, pred.data = train_oversampled, x.var = "product")
partialPlot(target.rf, pred.data = train_oversampled, x.var = "PC1")
partialPlot(target.rf, pred.data = train, x.var = "PC2")

# # Using the ranger package
# 
# target_default <- ranger(
#   formula    = target ~ ., 
#   data       = train_oversampled, 
#   num.trees  = 1000,
#   mtry       = 7,
#   respect.unordered.factors = 'order',
#   verbose    = FALSE,
#   probability=TRUE,
#   importance = 'permutation',
#   seed       = 123
# )
# 
# 
# #Prediction on training data
# 
# pred.data <- predict(target_default, dat = test_oversampled,)
# table(pred.data$predictions)
# 
# #Prediction on test data
# pred.data <- predict(target_default, dat = test,)
# table(pred.data$predictions)
# 
# 
# #ROC Curve ,Gain Chart and K-S Chart for test data and AUC values 0.925
# 
# target.test.random = predict(target_default,test, probability=TRUE)
# pred = prediction(target.test.random[,2], test$target)
# perf = performance(pred, "tpr", "fpr")
# plot(perf, colorize=TRUE)
# 
# target.test.random
# #Partial Dependencies Plot
# 
# target_default %>%
#   pdp::partial(pred.var = "product", train = as.data.frame(train_oversampled)) %>%
#   autoplot()
# 
# 
# # Extract tibble of variable importance scores
# vip::vi(target_default)
# 
# # Construct ggplot2-based variable importance plot
# vip::vip(target_default, num_features = 10)

###Boosting
consumer.boost = boosting(target~., data = train_oversampled, boos = TRUE, mfinal = 70,
                      control = rpart.control(cp = 0.01, maxdepth = 5))
save(consumer.boost, file = "bank.boost.Rdata")


#Prediction on training data

pred.tree1 <- predict(consumer.boost, train_oversampled, type="class", na.action = na.omit)
table(train_oversampled$target, pred.tree1, dnn=c("Truth","Predicted"))

#Prediction on test data
pred.test.tree <- predict(consumer.boost, test, type="class")
table(test$target, pred.test.tree, dnn=c("Truth","Predicted"))

# Testing AUC, ROC, Gain Chart and KS-plot
pred.customer.boost= predict(consumer.boost, newdata = test)
table(test$target, pred.customer.boost, dnn=c("Truth","Predicted"))
pred <- prediction(pred.customer.boost$prob[,2], test$target)
perf <- performance(pred, "tpr", "fpr")
plot(perf, colorize=TRUE)
#Get the AUC
unlist(slot(performance(pred, "auc"), "y.values")) 

gain <- performance(pred, "tpr", "rpp")
plot(gain, main = "Gain Chart")

ks=max(attr(perf,'y.values')[[1]]-attr(perf,'x.values')[[1]])
plot(perf,main=paste0(' KS=',round(ks*100,1),'%'))
lines(x = c(0,1),y=c(0,1))
print(ks); 


# Extract tibble of variable importance scores
vip::vi(consumer.boost)

# Construct ggplot2-based variable importance plot
vip::vip(consumer.boost, num_features = 10)

#Patial dependencies plot
partialPlot(consumer.boost, pred.data = train, x.var = "product")
partialPlot(consumer.boost, pred.data = train, x.var = "PC1")
partialPlot(consumer.boost, pred.data = train, x.var = "PC2")

# Construct ggplot2-based variable importance plot
summary(bank.boost)

write.csv(test, file = "test.csv", row.names = FALSE)
