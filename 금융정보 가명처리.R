setwd("C:/Users/ehdcj/Downloads/data-20210713T053959Z-001/data/Exercise")
dsn=read.csv("금융정보.csv", header=T, fileEncoding="utf-8")
library("dplyr")

#컬럼 삭제
dsn=select(dsn, -c(ID,Name,BirthDate))

#나이 범주화
dsn$AgeGrp<-NA
dsn$AgeGrp<-ifelse(dsn$Age<20,0,dsn$AgeGrp)
dsn$AgeGrp<-ifelse(dsn$Age>=20 & dsn$Age<25,1,dsn$AgeGrp)
dsn$AgeGrp<-ifelse(dsn$Age>=25 & dsn$Age<30,2,dsn$AgeGrp)
dsn$AgeGrp<-ifelse(dsn$Age>=30 & dsn$Age<35,3,dsn$AgeGrp)
dsn$AgeGrp<-ifelse(dsn$Age>=35 & dsn$Age<40,4,dsn$AgeGrp)
dsn$AgeGrp<-ifelse(dsn$Age>=40 & dsn$Age<45,5,dsn$AgeGrp)
dsn$AgeGrp<-ifelse(dsn$Age>=45 & dsn$Age<50,6,dsn$AgeGrp)
dsn$AgeGrp<-ifelse(dsn$Age>=50 & dsn$Age<55,7,dsn$AgeGrp)
dsn$AgeGrp<-ifelse(dsn$Age>=55 & dsn$Age<60,8,dsn$AgeGrp)
dsn$AgeGrp<-ifelse(dsn$Age>=60,9,dsn$AgeGrp)
dsn$Age<-dsn$AgeGrp
dsn = select(dsn, -c(AgeGrp)) 

#특이정보 처리(범주형 변수)
table(dsn$Job)
dsn$Job=ifelse(dsn$Job=="국회의원", "급여_공공",dsn$Job)
table(dsn$job)

#특이정보 처리(연속형 변수) --> 범주화로 처리 혹은 특이값이 아닌 최대값으로 변경 대체
boxplot(dsn$Nodebt)
boxplot.stats(dsn$Nodebt)$out
result<-boxplot.stats(dsn$Nodebt)
result
dsn$Nodebt<-ifelse(dsn$Nodebt>30,30,dsn$Nodebt)
boxplot(dsn$Nodebt)

#Rounding(천원단위)
table(dsn$Income)
dsn$Income<-round(dsn$Income/10**3, digit=0)*10**3
table(dsn$Overdue)
dsn$Overdue<-round(dsn$Overdue/10**2,digit=0)*10**2
boxplot(dsn$Income)
boxplot(dsn$Overdue)

#Rounding(정수화)
dsn$CreditScore<-round(dsn$CreditScore)

write.csv(dsn,file="금융정보_Pseudonymization.csv",row.names=FALSE, fileEncoding='utf-8')
