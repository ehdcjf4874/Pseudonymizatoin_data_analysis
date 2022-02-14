
# model : cart
# install.packages(c("rpart","caret"))
library(rpart)
library(caret)

install.packages("randomForest")
library(randomForest)

install.packages("gbm")
library(gbm)

install.packages("xgboost")
library(xgboost)

# read data
german_credit <- read.csv("german credit data.csv")

str(german_credit)


# change type of each variable

table(german_credit$existcr)
german_credit$existcr<-ifelse(german_credit$existcr<3,german_credit$existcr,3)
german_credit$existcr<-as.factor(german_credit$existcr)
table(german_credit$existcr)

german_credit$checking <- as.factor(german_credit$checking)
german_credit$duration<- as.numeric(german_credit$duration)
german_credit$history<- as.factor(german_credit$history)
german_credit$purpose<- as.factor(german_credit$purpose)
german_credit$amount<- as.numeric(german_credit$amount)
german_credit$savings<- as.factor(german_credit$savings)
german_credit$employed<- as.factor(german_credit$employed)
german_credit$installp<- as.factor(german_credit$installp)
german_credit$marital<- as.factor(german_credit$marital)
german_credit$coapp<- as.factor(german_credit$coapp)
german_credit$resident<- as.factor(german_credit$resident)
german_credit$property<- as.factor(german_credit$property)
german_credit$age<- as.numeric(german_credit$age)
german_credit$other<- as.factor(german_credit$other)
german_credit$housing<- as.factor(german_credit$housing)
german_credit$existcr<- as.factor(german_credit$existcr)
german_credit$job<- as.factor(german_credit$job)
german_credit$depends<- as.factor(german_credit$depends)
german_credit$telephon<- as.factor(german_credit$telephon)
german_credit$foreign<- as.factor(german_credit$foreign)
german_credit$good_bad<- as.factor(german_credit$good_bad)



# data split

nobs<-nrow(german_credit)

set.seed(345)

ntrain<-round(0.7*nobs)
nvalid<-nobs-ntrain

c(ntrain,nvalid)

pick1 <- sample(1:nobs, ntrain, replace=F)
train <- german_credit[pick1,]
valid <- german_credit[-pick1,]


tree1 <- rpart(good_bad ~., data = train)
par(xpd = NA) # otherwise on some devices the text is clipped
plot(tree1)
text(tree1, digits = 3)

print(tree1, digits = 2)


t <- rpart(good_bad ~age, data = train,cp=0)
par(xpd = NA) # otherwise on some devices the text is clipped
plot(t)
text(t, digits = 3)

print(t, digits = 2)






# 사전 pruning

# parameter control

ctrl <- rpart.control(minsplit = 10, minbucket = 5, cp = 0.01, 
             maxdepth = 6)

tree2 <- rpart(good_bad ~., data = train,control=ctrl)
plot(tree2)
text(tree2, digits = 3)

print(tree2, digits = 2)


ctrl <- rpart.control(minsplit = 10, minbucket = 5, cp = 0.0, 
             maxdepth = 6)

tree2 <- rpart(good_bad ~., data = train,control=ctrl)
plot(tree2)
text(tree2, digits = 3)

print(tree2, digits = 2)

# 사후 Pruning

plotcp(tree1)

tree1$cptable

ptree<-prune(tree1, cp= tree1$cptable[6,"CP"])
plot(ptree)
text(ptree)
print(ptree)

# cross-validation을 이용한 tree


train2 <- rbind(train,valid)
set.seed(12)
credit.cv <- train(
  good_bad ~., data = train2, method = "rpart",
  trControl = trainControl("cv", number = 10),
  tuneLength = 10
  )


plot(credit.cv)

credit.cv$bestTune

# Plot the final tree model

ctrl <- rpart.control(cp = credit.cv$bestTune)
tree3 <- rpart(good_bad ~., data = train2,control=ctrl)
plot(tree3)
text(tree3, digits = 3)
print(tree3,  digits = 2)




# 평가

pred.class <- predict(tree1,newdata=valid, type="class")
pred.prob <- predict(tree1,newdata=valid)
pred.class2 <- ifelse(pred.prob[,"bad"]>0.5,"bad","good")
head(pred.prob)
table(pred.class,pred.class2)
# 분할표
confusionMatrix(pred.class,valid$good_bad)


pred.class <- predict(ptree,newdata=valid, type="class")
confusionMatrix(pred.class,valid$good_bad)


# bagging tree

set.seed(1234)
bagging <- train(good_bad ~., 
   data = train, 
   method = "treebag",
   nbagg=10,
   trControl = trainControl(method = "cv", number = 10),
   metric = "Accuracy"
)

bagging$finalModel

bagging

summary(bagging)

plot(varImp(bagging))


# random forest


set.seed(1234)
rf <- randomForest( good_bad ~., data=train, 
	mtry = floor(sqrt(21)), 
	ntree = 500, importance = T)

rf
summary(rf)

ranf <- train(
   good_bad ~., 
   data = train, 
   method = "rf",
   metric = "Accuracy",
   ntree=100,
   tuneGrid = expand.grid(mtry = 1:10), # searching around mtry=4
   trControl = trainControl(method = "cv", number = 10)
)

ranf

plot(ranf)

plot(varImp(ranf))




# Gradient Boosting

gradb <- train(
   good_bad ~., 
   data = train, 
   method = "gbm",
   metric = "Accuracy",
   tuneLength = 5,
   trControl = trainControl(method = "cv", number = 10)
)

gradb

plot(gradb)



# XGBoost

xgb <- train(
   good_bad ~., 
   data = train, 
   method = "xgbTree",
   metric = "Accuracy",
   tuneLength = 5,
   trControl = trainControl(method = "cv", number = 10)
)

xgb 

plot(xgb )

plot(varImp(xgb ))




