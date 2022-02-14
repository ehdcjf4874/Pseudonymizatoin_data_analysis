library(dplyr)
library(gmodels)
library(sqldf)
library(lubridate)
setwd("C:/Users/ehdcj/Downloads/data-20210713T053959Z-001/data/Challenge")


# Data Reading
Cases<-read.csv("CoronaCases.csv", header=TRUE)
Patient<-read.csv("CoronaPatientInfo.csv", header=TRUE)

# Data Explore
sort(table(Patient$contact_number))
sort(table(Patient$province))
#식별자 patient_id 삭제
Patient=select(Patient, -c(patient_id))

#공백 삭제
Patient = Patient %>% filter(infection_case != '')
Patient$contact_number = ifelse(Patient$contact_number == '', -1, Patient$contact_number)
Patient_rm = Patient %>% filter(contact_number != '')

sort(table(Patient$contact_number))

table(Patient$infection_case)

#이상치 삭제
Patient=filter(Patient, c(!contact_number %in% c("6100000098", "6100000099" )))

Patient$contact_number = as.numeric(Patient$contact_number)

sort(table(Patient$contact_number))
boxplot(Patient$contact_number)

Patient$contact_number <- ifelse(Patient$contact_number < 50 & Patient$contact_number >=0, 0, Patient$contact_number)
Patient$contact_number <- ifelse(Patient$contact_number >= 50, 1, Patient$contact_number)
CrossTable(x = Patient$infection_case, y =Patient$contact_number , prop.t=FALSE, expected=TRUE, chisq =TRUE)

#Patient$infection_case를 factor형으로
Patient$infection_case <- as.factor(Patient$infection_case)
Patient_died = Patient %>% filter(state=='deceased') 
table(Patient_died$age)
a=as.numeric(nrow(Patient_died %>%filter(age=='30s'))/ nrow(Patient %>% filter(age=='30s')) * 100)
b=as.numeric(nrow(Patient_died %>%filter(age=='40s'))/ nrow(Patient %>% filter(age=='40s')) * 100)
c=as.numeric(nrow(Patient_died %>%filter(age=='50s'))/ nrow(Patient %>% filter(age=='50s')) * 100)
d=as.numeric(nrow(Patient_died %>%filter(age=='60s'))/ nrow(Patient %>% filter(age=='60s')) * 100)
e=as.numeric(nrow(Patient_died %>%filter(age=='70s'))/ nrow(Patient %>% filter(age=='70s')) * 100)
f=as.numeric(nrow(Patient_died %>%filter(age=='80s'))/ nrow(Patient %>% filter(age=='80s')) * 100)
g=as.numeric(nrow(Patient_died %>%filter(age=='90s'))/ nrow(Patient %>% filter(age=='90s')) * 100)
combine=c(a,b,c,d,e,f,g)
barplot(combine,names=c("30s","40s","50s","60s","70s","80s","90s"))
t.test(Patient$contact_number~Patient$infection_case)

Patient_symtom = Patient %>% filter(symptom_onset_date != "")
Patient_symtom$consumed = difftime(as.Date(Patient_symtom$confirmed_date), as.Date(Patient_symtom$symptom_onset_date))
Patient_symtom$consumed = as.numeric(Patient_symtom$consumed/86400)

#범주화
Patient_symtom$temp = NA
Patient_symtom$temp = ifelse(Patient_symtom$consumed<=0 ,0 ,Patient_symtom$temp)
Patient_symtom$temp = ifelse(Patient_symtom$consumed>0 & Patient_symtom$consumed < 5, 1, Patient_symtom$temp)
Patient_symtom$temp = ifelse(Patient_symtom$consumed >= 5 & Patient_symtom$consumed < 10, 2, Patient_symtom$temp)
Patient_symtom$temp = ifelse(Patient_symtom$consumed >= 10 & Patient_symtom$consumed < 15, 3, Patient_symtom$temp)
Patient_symtom$temp = ifelse(Patient_symtom$consumed >= 15& Patient_symtom$consumed < 20, 4, Patient_symtom$temp)
Patient_symtom$temp = ifelse(Patient_symtom$consumed >= 20, 5, Patient_symtom$temp)
Patient_symtom$consumed = Patient_symtom$temp
Patient_symtom = select(Patient_symtom, -c(temp))
View(Patient_symtom)
table(Patient_symtom$consumed)
CrossTable(x = Patient_symtom$age, y =Patient_symtom$consumed , prop.t=FALSE, expected=TRUE, chisq =TRUE)

#갯수 counting
temp = count(Patient_symtom %>% filter(Patient_symtom$age == '20s'&consumed == 1))

Patient_symtom$count = NA
library(ggplot2)
#test <- data.frame(Patient_symtom$age, Patient_symtom$consumed)

#모자이크Plot(소모기간과 나이)
Patient_symtom = Patient_symtom %>% filter(age != '')
mosaicplot(~age + consumed, data=Patient_symtom, color=TRUE)

#월 추출
Patient$month = month(as.Date(Patient$confirmed_date))
Patient_fill = Patient %>% filter(month == c(3,4,5,6))

Patient_fill$temp = NA
Patient_fill$temp = ifelse(day(Patient_fill$confirmed_date) >=15 & day(Patient_fill$confirmed_date) <22 & Patient_fill$month == 3,0,Patient_fill$temp)
Patient_fill$temp = ifelse(day(Patient_fill$confirmed_date) >=22 & day(Patient_fill$confirmed_date) <29 & Patient_fill$month == 3,1,Patient_fill$temp)
Patient_fill$temp = ifelse((day(Patient_fill$confirmed_date) >=29 & Patient_fill$month == 3) | (day(Patient_fill$confirmed_date) <5 & Patient_fill$month==4),2,Patient_fill$temp)
Patient_fill$temp = ifelse(day(Patient_fill$confirmed_date) >= 5 & day(Patient_fill$confirmed_date) < 12 & Patient_fill$month == 4, 3, Patient_fill$temp)
Patient_fill$temp = ifelse(day(Patient_fill$confirmed_date) >= 12 & day(Patient_fill$confirmed_date) < 19 & Patient_fill$month == 4, 4, Patient_fill$temp)
Patient_fill$temp = ifelse(day(Patient_fill$confirmed_date) >= 19 & day(Patient_fill$confirmed_date) < 26 & Patient_fill$month == 4, 5, Patient_fill$temp)
Patient_fill$temp = ifelse((day(Patient_fill$confirmed_date) >=26 & Patient_fill$month == 4) | (day(Patient_fill$confirmed_date) <3 & Patient_fill$month==5),6,Patient_fill$temp)
Patient_fill$temp = ifelse(day(Patient_fill$confirmed_date) >=3 & day(Patient_fill$confirmed_date) <10 & Patient_fill$month == 5,7,Patient_fill$temp)
Patient_fill$temp = ifelse(day(Patient_fill$confirmed_date) >=10 & day(Patient_fill$confirmed_date) <17 & Patient_fill$month == 5,8,Patient_fill$temp)
Patient_fill$temp = ifelse(day(Patient_fill$confirmed_date) >=17 & day(Patient_fill$confirmed_date) <24 & Patient_fill$month == 5,9,Patient_fill$temp)
Patient_fill$temp = ifelse(day(Patient_fill$confirmed_date) >=24 & day(Patient_fill$confirmed_date) <=31 & Patient_fill$month == 5,10,Patient_fill$temp)
Patient_fill$temp = ifelse(day(Patient_fill$confirmed_date) >=1 & day(Patient_fill$confirmed_date) <7 & Patient_fill$month == 6,11,Patient_fill$temp)
Patient_fill$temp = ifelse(day(Patient_fill$confirmed_date) >=7 & day(Patient_fill$confirmed_date) <14 & Patient_fill$month == 6,12,Patient_fill$temp)
Patient_fill$temp = ifelse(day(Patient_fill$confirmed_date) >=14 & day(Patient_fill$confirmed_date) <21 & Patient_fill$month == 6,13,Patient_fill$temp)
Patient_fill$temp = ifelse(day(Patient_fill$confirmed_date) >=21 & day(Patient_fill$confirmed_date) <28 & Patient_fill$month == 6,14,Patient_fill$temp)
hist(Patient_fill$temp)
Patient_fill_temp = Patient_fill %>% filter(day(Patient_fill$confirmed_date) >=24 & day(Patient_fill$confirmed_date) <=31 & Patient_fill$month == 5)
table(Patient_fill_temp$infection_case)

#모델
nobs<-nrow(Patient)

set.seed(125)

ntrain<-round(0.4*nobs)
nvalid<-round(0.3*nobs)
ntest<-nobs-ntrain-nvalid

c(ntrain,nvalid,ntest)

pick1 <- sample(1:nobs, ntrain, replace=F)
train <- Patient[pick1,]
pick2 <- sample(1:(nobs-ntrain), nvalid, replace=F)
valid <- Patient[-pick1,][pick2,]
test <- Patient[-pick1,][-pick2,]
nrow(train)
nrow(valid)
nrow(test)

table(train$existcr)
table(valid$existcr)
table(test$existcr)

write.csv(Patient_symtom,"Patient_symtom.csv")
