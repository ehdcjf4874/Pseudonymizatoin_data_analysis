# change the working directory
setwd("e:\\데이터마이닝")

# read data
german_credit <- read.csv("german credit data.csv")

str(german_credit)

summary(german_credit)

# change type of each variable

table(german_credit$existcr)
german_credit$existcr<-ifelse(german_credit$existcr<3,german_credit$existcr,3)
table(german_credit$existcr)

table(german_credit$purpose)
german_credit$purpose <- ifelse(german_credit$purpose=="X","10",german_credit$purpose)
table(german_credit$purpose)

german_credit$checking <- as.numeric(german_credit$checking)
german_credit$duration<- as.numeric(german_credit$duration)
german_credit$history<- as.numeric(german_credit$history)
german_credit$purpose<- as.numeric(german_credit$purpose)
german_credit$amount<- as.numeric(german_credit$amount)
german_credit$savings<- as.numeric(german_credit$savings)
german_credit$employed<- as.numeric(german_credit$employed)
german_credit$installp<- as.numeric(german_credit$installp)
german_credit$marital<- as.numeric(german_credit$marital)
german_credit$coapp<- as.numeric(german_credit$coapp)
german_credit$resident<- as.numeric(german_credit$resident)
german_credit$property<- as.numeric(german_credit$property)
german_credit$age<- as.numeric(german_credit$age)
german_credit$other<- as.numeric(german_credit$other)
german_credit$housing<- as.numeric(german_credit$housing)
german_credit$existcr<-as.numeric(german_credit$existcr)
german_credit$job<- as.numeric(german_credit$job)
german_credit$depends<- as.numeric(german_credit$depends)
german_credit$telephon<- as.numeric(german_credit$telephon)
german_credit$foreign<- as.numeric(german_credit$foreign)

# change type and the order in good_bad
german_credit$good_bad <- factor(german_credit$good_bad,level=c("good","bad"))

str(german_credit)

library(moments)

# 연속형변수의 대칭화 & 표준화

# 대칭화

german1 <- german_credit

hist(german1$amount)

summary(german1$amount)

library(MASS)
library(caret)
library(moments)

agostino.test(german_credit$amount)
boxcox(german1$amount~1,lambda=seq(-2,2,0.25),plotit=T)
german1$amount<-log(german1$amount)

skewness(german1$amount)
agostino.test(german1$amount)

hist(german1$duration)
agostino.test(german_credit$duration)
boxcox(german1$duration~1,lambda=seq(-2,2,0.25),plotit=T)
german1$duration<-log(german1$duration)

skewness(german1$duration)
agostino.test(german1$duration)

skewness(german1$age)
agostino.test(german1$age)

hist(german1$age)
boxcox(german1$age~1,lambda=seq(-2,2,0.25),plotit=T)
german1$age <- (german1$age)^(-1/2)

skewness(german1$age)
agostino.test(german1$age)

# 변수 구분: factor, numeric, target

names(german1)
xvar <- names(german1[,-ncol(german1)]) # get names for explanatory variables
xnvar <-c("duration","amount","age")	# numeric variables
xfvar <-xvar[!(xvar %in% c("duration","amount","age"))]	# factor variables
yvar <- names(german1)[ncol(german1)]	# target variable




# 연속형 변수의 표준화

nstdProcess <- preProcess(german1[,xnvar], method=c("center", "scale"))
german1[,xnvar] <- predict(nstdProcess, german1[,xnvar])
library(psych)
describe(german1[,c("duration","amount","age")])



# dummy variable 생성

german2 <- german1
str(german2)
german2[,xfvar] <- sapply(german2[,xfvar],as.factor)
str(german2)

# create dummy variables for factor variables
f <- as.formula(paste('~',paste(xfvar,collapse = '+')))
dummy <- model.matrix(f,data=german2)
head(dummy)

german2 <-cbind(german1[,c(yvar,xnvar)],dummy[,-1])
head(german2)
str(german2)



# 범주형 변수의 표준화

fstdProcess <- preProcess(german1[,xfvar], method=c("range"))
german1[,xfvar] <- predict(fstdProcess, german1[,xfvar])

table(german1$checking)

head(german1)


# data split

nobs<-nrow(german1)
#nobs<-nrow(german1)

set.seed(345)

ntrain<-round(0.7*nobs)
nvalid<-nobs-ntrain

c(ntrain,nvalid)

pick1 <- sample(1:nobs, ntrain, replace=F)

train <- german1[pick1,]
valid <- german1[-pick1,]
# train <- german2[pick1,]
# valid <- german2[-pick1,]




# model: neural network using neuralnet

# install.packages("neuralnet")
library(neuralnet)

# create formula

#var <- names(german2)
var <- names(german1)
f <- as.formula(paste('good_bad~',paste(xvar,collapse = '+')))
f


# model 1

?neuralnet
set.seed(123)
neural1 <- neuralnet(f, data=train, hidden=2, threshold=0.01, 
                     linear.output = F, act.fct="tanh")
plot(neural1)
head(neural1$net.result[[1]])

# misclassification rate

pred <- predict(neural1,newdata=train)
pred.class <- ifelse(pred[,1]>0.5,"bad","good")
table(pred.class,train$good_bad)
mean(pred.class != train$good_bad) # misclassification rate for train


pred <- predict(neural1,newdata=valid)
pred.class <- ifelse(pred[,1]>0.5,"bad","good")
table(pred.class,valid$good_bad)
mer1 <- mean(pred.class != valid$good_bad) # misclassification rate for target
mer1

# model 2

set.seed(123)
neural2 <- neuralnet(f, data=train, hidden=c(2,2),threshold=0.01, 
                     linear.output = FALSE, act.fct="tanh",likelihood=T)
plot(neural2,rep = 'best')

# misclassification rate

pred <- predict(neural2,newdata=train)
pred.class <- ifelse(pred[,1]>0.5,"bad","good")
mean(pred.class != train$good_bad) # misclassification rate for train

pred <- predict(neural2,newdata=valid)
pred.class <- ifelse(pred[,1]>0.5,"bad","good")
table(pred.class,valid$good_bad)
mer2 <- mean(pred.class != valid$good_bad) # misclassification rate for target
mer2

# model 3

set.seed(10)

neural3 <- neuralnet(f,data=train,hidden=4,
                    threshold = 0.01,act.fct="tanh",likelihood=T, 
                    algorithm = "rprop+")
plot(neural3)

# misclassification rate

pred <- predict(neural3,newdata=train)
pred.class <- ifelse(pred[,1]>0.5,"bad","good")
mean(pred.class != train$good_bad) # misclassification rate for train

pred <- predict(neural3,newdata=valid)
pred.class <- ifelse(pred[,1]>0.5,"bad","good")
table(pred.class,valid$good_bad)
mer3 <- mean(pred.class != valid$good_bad) # misclassification rate for target
mer3

# ROC chart & AUC

library(ROCR)
neural2.p <-predict(neural2, newdata=valid)
neural2.pred <- prediction(neural2.p[,2], valid$good_bad)
neural2.roc <- performance(neural2.pred, "tpr", "fpr")   
plot(neural2.roc, colorize=TRUE) # ROC-chart

performance(neural2.pred, "auc")@y.values[[1]] # AUC


#lift chart

neural2.lift1 <- performance(neural2.pred, "lift", "rpp")
plot(neural2.lift1, colorize=TRUE)
