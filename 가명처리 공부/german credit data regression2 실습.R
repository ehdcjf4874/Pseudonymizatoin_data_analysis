# 작업 디렉토리 설정
setwd("e:\\데이터마이닝")

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

set.seed(125)

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

mars1 <- earth(  good_bad ~ .,degree=2, data = train, nfold=10)


print(mars1)
summary(mars1)

plot(mars1)




hyper_grid <- expand.grid(
  degree = 1:3, 
  nprune = seq(2, 100, length.out = 10) %>% floor()
  )

tuned_mars <- train(
  x = subset(train, select = -good_bad),
  y = train$good_bad,
  method = "earth",
  metric = "Accuracy",
  trControl = trainControl(method = "cv", number = 10),
  tuneGrid = hyper_grid
)


tuned_mars$bestTune

plot(tuned_mars)



mars1 <- mars(  train[,-21],train[,21])

plotmo(mars1)


# variable importance plots
p1 <- vip(tuned_mars, num_features = 40, bar = FALSE, value = "gcv") + ggtitle("GCV")
p2 <- vip(tuned_mars, num_features = 40, bar = FALSE, value = "rss") + ggtitle("RSS")
gridExtra::grid.arrange(p1, p2, ncol = 2)


plot(p1)

print(mars1)
summary(mars1)

plot(mars1)
plotmo(mars1)

predict(mars1,valid,type="response")

















# full model (logistic regression)

t<-names(train)
t
t[21]
var<-t[-21]

good_bad~checking+age+duaration+existcr

f<-as.formula(paste("good_bad~",paste(var,collapse="+")))

full <- glm(f,family=binomial(link="logit"),data=train)
summary(full)
AIC(full)
BIC(full)

# variable selection

install.packages("MASS")
library(MASS)


null <- glm(good_bad ~ 1, family = binomial, data=train)
summary(null)


# forward 

# for AIC
for_AIC <-stepAIC(null, scope=list(lower=null, upper=full), k = 2, direction="forward")
AIC(for_AIC) 

# for BIC
#for_BIC <-stepAIC(null, scope=list(lower=null, upper=full), k = log(nrow(train)), direction="forward")
#formula =  good_bad ~ checking + duration

# backward

# AIC
back_AIC  <- stepAIC(full, k = 2, direction="backward")
# good_bad ~ checking + duration + amount + savings + 
#    installp + marital + coapp + age + existcr + depends + lage36 + 
#    history2
AIC(back_AIC)
back_AIC$anova


# stepwise

# AIC
step_AIC  <- stepAIC(null, scope=list(lower=null, upper=full), k = 2, direction="both")
# good_bad ~ checking + duration + history2 + installp + marital + 
#    amount + existcr + coapp + savings + depends
AIC(step_AIC)
step_AIC$anova



AIC(full)
AIC(for_AIC)
AIC(back_AIC)
AIC(step_AIC)#*



# 오분류 확률이 performance 측도

fitted <- predict(full,newdata=valid,type='response')
fitted <- ifelse(fitted > 0.5,1,0)
error_full <- mean(fitted != ifelse(valid$good_bad=="bad",1,0))
error_full



level(pclass)

# confusion matrix

install.packages("caret")
library(caret)

install.packages("e1071")
library(e1071)

prob <- predict(full,newdata=valid,type='response')
pclass <- as.factor(ifelse(prob > 0.5,"bad","good"))
confusionMatrix(pclass,valid$good_bad, positive="bad")
mer_full <- mean(pclass != valid$good_bad)
mer_full

prob <- predict(for_AIC,newdata=valid,type='response')
pclass<- as.factor(ifelse(prob > 0.5,"bad","good"))
confusionMatrix(pclass,valid$good_bad)
mer_for <- mean(pclass != valid$good_bad)

prob <- predict(back_AIC,newdata=valid,type='response')
pclass<- as.factor(ifelse(prob > 0.5,"bad","good"))
confusionMatrix(pclass,valid$good_bad)
mer_back <- mean(pclass != valid$good_bad)

prob <- predict(step_AIC,newdata=valid,type='response')
pclass<- as.factor(ifelse(prob > 0.5,"bad","good"))
confusionMatrix(pclass,valid$good_bad)
mer_step <- mean(pclass != valid$good_bad)

mer_full
mer_for
mer_back
mer_step

# ROC

install.packages("pROC")
library(pROC)

rocplot <- roc(valid$good_bad~prob)
plot.roc(rocplot,legacy.exes=TRUE)
auc(rocplot)


# ROC chart & AUC
install.packages("ROCR")
library(ROCR)
pred <- prediction(prob, ifelse(valid$good_bad=="bad",1,0))
roc <- performance(pred, "tpr", "fpr")   
plot(roc, colorize=TRUE) # ROC-chart

performance(pred, "auc")@y.values[[1]] # AUC

lift <- performance(pred,"lift","rpp")
plot(lift,main="lift curve")


# 성능 예측


prob <- predict(step_AIC,newdata=test,type='response')
pclass<- as.factor(ifelse(prob > 0.5,"bad","good"))
confusionMatrix(pclass,valid$good_bad)
mer_step <- mean(pclass != valid$good_bad)





