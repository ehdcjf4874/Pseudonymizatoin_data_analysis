# 작업 디렉토리 설정
setwd("e:\\데이터마이닝")

# 데이터 읽기
german_credit <- read.csv("german credit data.csv")

str(german_credit)

# 변수 정의에 맞게 변수 타입 변경
german_credit$checking <- as.factor(german_credit$checking)
german_credit$duration<- as.numeric(german_credit$duration)
german_credit$history<- as.factor(german_credit$history)
german_credit$purpose<- as.factor(german_credit$purpose)
german_credit$amount<- as.numeric(german_credit$amount)
german_credit$savings<- as.factor(german_credit$savings)
german_credit$employed<- as.factor(german_credit$employed)
german_credit$installp<- as.numeric(german_credit$installp)
german_credit$marital<- as.factor(german_credit$marital)
german_credit$coapp<- as.factor(german_credit$coapp)
german_credit$resident<- as.numeric(german_credit$resident)
german_credit$property<- as.factor(german_credit$property)
german_credit$age<- as.numeric(german_credit$age)
german_credit$other<- as.factor(german_credit$other)
german_credit$housing<- as.factor(german_credit$housing)
german_credit$existcr<- as.numeric(german_credit$existcr)
german_credit$job<- as.factor(german_credit$job)
german_credit$depends<- as.numeric(german_credit$depends)
german_credit$telephon<- as.factor(german_credit$telephon)
german_credit$foreign<- as.factor(german_credit$foreign)
german_credit$good_bad<- as.factor(german_credit$good_bad)

summary(german_credit)

# 관심이 bad이기 때문에 이에 대한 변수를 생성 (0: good, 1: bad)
german_credit$response<- ifelse(german_credit$good_bad=="bad",1,0)
german_credit$response<-as.factor(german_credit$response)

table(german_credit$response)

# 수치형변수에서 값의 개수 확인 및 범주형으로 분석할 것인지를 결정

table(german_credit$installp)
german_credit$installp<-as.factor(german_credit$installp)

table(german_credit$resident)
german_credit$resident<-as.factor(german_credit$resident)

table(german_credit$existcr)
german_credit$existcr<-as.factor(german_credit$existcr)

table(german_credit$depends)
german_credit$depends<-as.factor(german_credit$depends)

str(german_credit)

# 연속형 변수에 대한 분석; age, duration, amount

# age

# 단변량 분석

# histogram

# hist(german_credit$age)
with(german_credit,hist(age))
# 왼쪽으로 쏠린 분포를 하고 있다.

with(german_credit,hist(log(age)))

# 3.3과 3.4 사이에 관측값이 적어 이 구간을 중심으로 두 그룹으로 나눌 수도 
# 있으나 이 정도가 아주 심하지 않다고 생각되면 무시하고 분석할 수도 있다.

# density plot

with(german_credit,plot(density(age)))
with(german_credit,plot(density(log(age),bw=0.1)))
# 히스토그램과 비슷한 경향을 보인다.

# descriptive statistics

install.packages("psych")
library(psych)

describe(german_credit$age)
describe(log(german_credit$age))
# 평균은 36세, 중앙값은 33세인 것을 알 수 있다. 중앙값이 평균보다 
# 작은 것은 분포가 왼쪽으로 쏠려 있기 때문으로 보인다. 왜도는 1.02로
# 왼쪽으로 쏠린 분포인 것을 알 수 있다.
# log를 취하면 왜도가 0.42로 쏠린 정도가 완화되었지만 아직 왼쪽으로 
# 쏠린 분포를 하고 있는 것을 알 수 있다.


# 이변량 분석; age vs. response

# boxplot

with(german_credit,plot(age~response))
# response의 각 범주에 따라 분포가 왼쪽으로 쏠린 분포를 가지고 있다.
# 중심의 위치가 0일 때가 1일 때보다 크다.


# 히스토그램
with(german_credit,hist(age[which(response==0)]))
with(german_credit,hist(age[which(response==1)]))
# overlaying
summary(german_credit$age)
with(german_credit,
	hist(age[which(response==0)],xlim=c(15,80),ylim=c(0,0.05),
		freq=F,col=rgb(0,0,1,0.5),
		main="good과 bad의 나이 분포 비교",xlab="나이",ylab="밀도"
	)
)
with(german_credit,
	hist(age[which(response==1)],add=T,
		freq=F,col=rgb(1,0,0,0.5)
	)
)
# bad일 때의 분포가 good일 때의 분포보다 왼쪽으로 더 쏠려 보인다.

# density plot
d0<-with(german_credit,density(age[which(response==0)],bw=4))
plot(d0)
d1<-with(german_credit,density(age[which(response==1)]))
plot(d1)
d0
d1

plot(d0, xlim=c(5,90),ylim=c(0,0.05),col="blue",lwd=3,
	main="나이의 분포",xlab="나이",ylab="밀도")
with(german_credit,abline(v=mean(age[which(response==0)]),col="blue",
	lty=2,lwd=3))
lines(d1,col="red",lwd=3)
with(german_credit,abline(v=mean(age[which(response==1)]),col="red",
	lty=2,lwd=3))
legend("topright",c("good","bad"),lty=1,lwd=3,col=c("blue","red"))
# bad일 때의 분포가 왼쪽으로 좀 더 쏠려 있고 중심의 위치도 왼쪽에 위치 한
# 것으로 보인다.


# 기술 통계량

describeBy(german_credit$age,group=german_credit$response)
# 기술통계량에 의하면 평균이 good에서 더 높고 왜도는 bad에서 더 높다. 

# 검정

ks.test(german_credit[which(german_credit$response==1),"age"],
	  german_credit[which(german_credit$response==0),"age"])
# p-값이 매우 작으므로 good와 bad에서 나이의 분포가 다르다고 할 수 있다.


# scatter plot

with(german_credit,
	plot(age,as.integer(response), main="나이와 신용의 산점도",
		xlab="나이",ylab="신용")
)
with(german_credit,
	lines(smooth.spline(age,response),col="red",lwd=2)
)

# 나이가 적을 때 신용도가 낮고 나이가 먹음에 따라 신용도가 높아지고
# 40 이후에는 신용도가 일정해지는 경향을 가지고 있다.

# scatter plot; log 변환

with(german_credit,
	plot(log(age),as.integer(response), main="나이와 신용의 산점도",
		xlab="나이",ylab="신용")
)
with(german_credit,
	lines(smooth.spline(log(age),response),col="red",lwd=2)
)
# log 취하기 이전과 비슷한 경향을 보인다.

# categorization (discretization)

library(arules)

cage<-discretize(german_credit$age,method="frequency",breaks=10)
table(cage)
table(german_credit$age)
cage<-discretize(german_credit$age,method="fixed",
	breaks=c(0,24,26,28,30,33,36,39,45,52,100))
table(cage)

library(psych)

mosaicplot(table(cage,german_credit$response))

cage2<-discretize(german_credit$age,method="frequency",breaks=5)
table(cage2)
mosaicplot(table(cage2,german_credit$response))

library(gmodels)

table_age<-table(cage2,german_credit$response)

CrossTable(table_age,prop.t=F,prop.c=F,chisq=T)

CrossTable(table_age[-1,],prop.t=F,prop.c=F,chisq=T)

# 26살 미만에서 bad의 비율 다른 나이대에 비하여 높아보인다. 26살 미만을 제외한
# 다른 나이대에서는 chi-square 검정에 의하면 bad의 비율이 다르다고 보기 힘들다.
# 만약 이러한 경향을 반영하려면 26살 미만은 1, 26살 이상은 0으로 하는 가변수를
# 생성할 수 있다.

# 또 다른 측면에서는 26샇 미만에서 bad의 비율이 높고 [26,30), [30,36)에서 
# bad의 비율이 줄어드는 것처럼 보이고 36살 이상에서는 bad의 비율이 일정해
# 보인다. 이러한 경향을 맞는 변수를 차후에 생성할 필요가 있어 보인다.


# 변수변환

# ramp function
#t<-(german_credit$age>36)*(german_credit$age-36)
#plot(german_credit$age,t)

# 36세 이전까지 선형적으로 감소 36세 이후에는 0.
german_credit$age36 <- (german_credit$age<36)*(36-german_credit$age)
plot(german_credit$age,german_credit$age36)
german_credit$age36 <-as.numeric(german_credit$age36)

# log 변환
german_credit$lage <- log(german_credit$age)
german_credit$lage <- as.numeric(german_credit$lage)


# log(36세) 이전까지 선형적으로 감소 log(36세) 이후에는 0.
german_credit$lage36 <- (german_credit$lage<log(36))*(log(36)-german_credit$lage)
german_credit$lage36 <- as.numeric(german_credit$lage36)


# duration

with(german_credit,hist(duration))

with(german_credit,hist(duration,breaks=100))

t <- table(german_credit$duration)

# scatter plot matrix

pairs(german_credit[,c("age","duration","amount")])


# 범주형 변수

# history

#  단변량

barplot(table(german_credit$history),col=rainbow(4))
pie(table(german_credit$history),col=rainbow(4))
CrossTable(german_credit$history)


# 이변량 분석; history vs. response

table_history <- with(german_credit,table(response,history))

# barplot(table_history)
# barplot(table_history,beside=T)

mosaicplot(t(table_history),col=c("blue","red"))

CrossTable(t(table_history)[c(3,4),],prop.t=F,prop.c=F,chisq=T)

CrossTable(t(table_history)[c(1,2),],prop.t=F,prop.c=F,chisq=T)

# 4는 다른 범주보다 신용도가 높아보인다. 2와 3에서의 신용도가 비슷해보인다.
# 1과 2는 다른 범주에 비하여 신용도가 낮고 1과 2 사이에는 약간의 차이가 
# 있으나 chi-square test에 의하면 큰 차이가 없어 보이므로 비슷하다고 고려하고
# 아래와 같이 범주를 합친다.

german_credit$history2 <-
	ifelse(german_credit$history %in% c(0,1),"(0,1)",
	ifelse(german_credit$history %in% c(2,3),"(2,3)","4"))

german_credit$history2 <- as.factor(german_credit$history2)

table(german_credit$history2)
mosaicplot(table(german_credit$history2,german_credit$response),col=c("blue","red"))

CrossTable(table(german_credit$history2,german_credit$response),chisq=T)


# purpose


# 단변량

barplot(table(german_credit$purpose),col=rainbow(4))
pie(table(german_credit$purpose),col=rainbow(4))
CrossTable(german_credit$purpose)

# 이변량; purpose vs. response

table_purpose <- with(german_credit,table(purpose,response))

mosaicplot(table_purpose,col=c("blue","red"))

CrossTable(table_purpose,prop.t=F,prop.c=F,chisq=T)

CrossTable(table_purpose[c(1,3,5,6,9),],prop.t=F,prop.c=T,chisq=T)

CrossTable(table_purpose[c(7,10),],prop.t=F,prop.c=T,chisq=T)

CrossTable(table_purpose[c(2,4,8),],prop.t=F,prop.c=T,chisq=T)

german_credit$purpose2 <- with(german_credit,
	ifelse(purpose %in% c(0,2,4,5,9), "(0,2,4,5,9)",
	ifelse(purpose %in% c(1,3,8),"(1,3,8)","(6,X)")
))

german_credit$purpose2 <- as.factor(german_credit$purpose2)

table(german_credit$purpose2)

mosaicplot(table(german_credit$purpose2,german_credit$response),col=c("blue","red"))

CrossTable(table(german_credit$purpose2,german_credit$response),prop.t=F,prop.c=T,chisq=T)

german_credit$purpose2 <-as.factor(german_credit$purpose2)






