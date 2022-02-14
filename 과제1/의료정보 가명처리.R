setwd("C:/Users/ehdcj/OneDrive - 충북대학교/가명정보처리 실습파일/과제1")

dsn=read.csv("의료정보_Pseudonymization.csv", header=T, fileEncoding="utf-8")
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

#민감정보 이상치 삭제
sort(table(dsn$Dept))
dsn=filter(dsn, c(!Dept%in% c("BRES","COGE", " CORO",  "PAMD",  "PCHO",  "CHHO",  "SVNM" )))
sort(table(dsn$DCode))
sort(table(dsn$Medicine))
dns=filter(dsn, c(!Medicine%in% c("10180","10206","10567","10695","10808","10826","10838","10957","11096","11441","11592","11706","11725","11826","11832","12140","12525","12529","50398","51277","61379","65036","65055","90928","10411","10575","10642","10701","10760","10804","10942","11088","11094","11517","11523","11711","11715","11831","12126","12417","50631","51106","60680","62745","65027","65035","65102","10182","10517","10601","10805","10893","11104","11589","11714","11827","11828","11835","11993","12007","12291","12413","55007","61259","62749","65001","65110")))
