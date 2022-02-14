
setwd("C:/Users/ehdcj/Downloads/data-20210713T053959Z-001/data/Exercise")


df = read.csv("adultSalary.csv", header=TRUE)
library(synthpop)
sds <- syn(df)                # 재현정보 생성
synData = sds$syn

write.csv(synData, "syn_adultSalary.csv",row.names=F)

comp1 <- function(a, b){
  var_names = names(a)
  n = length(vars)
  for(i in 1:n){
    print(compare(b, a, vars = var_names[i]))
  }
}

comp1(df, sds)
library(nnet)

comp2 <- function(a,b){
  lout<-multinom(a$salary_class~a$age+a$workclass+a$marital_status+a$race+a$hours_per_works, data=a)
  pred<-predict(lout,a,type="class")
  print(table(pred, a$salary_class))
  lout<-multinom(b$salary_class~b$age+b$workclass+b$marital_status+b$race+b$hours_per_works, data=b)
  pred<-predict(lout,b,type="class")
  print(table(pred, b$salary_class))
}

comp2(df, synData)
