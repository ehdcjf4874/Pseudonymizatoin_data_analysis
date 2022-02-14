
library(magrittr)
library(earth)
library(MASS)
library(mda)
library(caret)

# 데이터 읽기
german_credit <- read.csv("german credit data.csv")

str(german_credit)


# 변수 정의에 맞게 변수 타입 변경
german_credit$checking <- as.factor(german_credit$checking)
german_credit$duration <- as.numeric(german_credit$duration)
german_credit$history <- as.factor(german_credit$history)
german_credit$purpose <- as.factor(german_credit$purpose)
german_credit$amount <- as.numeric(german_credit$amount)
german_credit$savings <- as.factor(german_credit$savings)
german_credit$employed <- as.factor(german_credit$employed)
german_credit$installp <- as.factor(german_credit$installp)
german_credit$marital <- as.factor(german_credit$marital)
german_credit$coapp <- as.factor(german_credit$coapp)
german_credit$resident <- as.factor(german_credit$resident)
german_credit$property <- as.factor(german_credit$property)
german_credit$age <- as.numeric(german_credit$age)
german_credit$other <- as.factor(german_credit$other)
german_credit$housing <- as.factor(german_credit$housing)
german_credit$existcr <- as.factor(german_credit$existcr)
german_credit$job <- as.factor(german_credit$job)
german_credit$depends <- as.factor(german_credit$depends)
german_credit$telephon <- as.factor(german_credit$telephon)
german_credit$foreign <- as.factor(german_credit$foreign)

german_credit$good_bad <- factor(german_credit$good_bad,level=c("good","bad"))
# german_credit$response <- factor(ifelse(german_credit$good_bad=="bad",1,0),levels=c(0,1),labels=c("good","bad"))



# data split

nobs<-nrow(german_credit)

set.seed(12)

ntrain<-round(0.4*nobs)
nvalid<-round(0.3*nobs)
ntest<-nobs-ntrain-nvalid

c(ntrain,nvalid,ntest)

pick1 <- sample(1:nobs, ntrain, replace=F)
train <- german_credit[pick1,]
pick2 <- sample(1:(nobs-ntrain), nvalid, replace=F)
valid <- german_credit[-pick1,][pick2,]
test <- german_credit[-pick1,][-pick2,]
nrow(train)
nrow(valid)
nrow(test)

table(train$existcr)
table(valid$existcr)
table(test$existcr)


str(train)

mars1 <- earth(  good_bad ~ .,degree=2, data = train)


print(mars1)
summary(mars1)

plot(mars1)

plotmo(mars1)


# tune the model

train2 <- rbind(train,valid)

hyper_grid <- expand.grid(
  degree = 1:2, 
  nprune = seq(2, 50, length.out = 10) %>% floor()
  )

tuned_mars <- train(
  x = subset(train2, select = -good_bad),
  y = train2$good_bad,
  method = "earth",
  metric = "Accuracy",
  trControl = trainControl(method = "cv", number = 10),
  tuneGrid = hyper_grid
)

plot(tuned_mars)

tuned_mars$bestTune




hyper_grid <- expand.grid(
  degree = 1:2, 
  nprune = 10:20
  )

tuned_mars <- train(
  x = subset(train, select = -good_bad),
  y = train$good_bad,
  method = "earth",
  metric = "Accuracy",
  trControl = trainControl(method = "cv", number = 5),
  tuneGrid = hyper_grid
)

plot(tuned_mars)

tuned_mars$bestTune

library(vip)
# variable importance plots
p1 <- vip(tuned_mars, num_features = 40, bar = FALSE, value = "gcv") + ggtitle("GCV")
p2 <- vip(tuned_mars, num_features = 40, bar = FALSE, value = "rss") + ggtitle("RSS")

p1
p2

grid.arrange(p1, p2, ncol = 2)


#final model
mars.final <- earth(  good_bad ~ .,degree=1, nprune=18, data = train2)

summary(mars.final)

print(mars.final)
summary(mars.final)

plot(mars.final)

plotmo(mars.final)




prob <- predict(mars.final,newdata=test,type='response')
pclass<- as.factor(ifelse(prob > 0.5,"bad","good"))
confusionMatrix(pclass,test$good_bad)
mer_MARS <- mean(pclass != test$good_bad)
mer_MARS

