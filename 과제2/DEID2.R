setwd("C:/Users/ehdcj/OneDrive - 충북대학교/가명정보처리 실습파일/과제2")
df <- read.csv("adult_salary_out.csv", header=TRUE)
summary(df)

# Quasi-Identifier: age, workclass, marital_status, race, sex, native_country
# sensitive variable: salary_class

library(sqldf)
freq <- sqldf("select count(*) from df group by age, workclass, marital_status, race, sex, native_country")
table(freq$`count(*)`)
sqldf("select age, count(*) from df group by age")
sqldf("select workclass, count(*) from df group by workclass")
sqldf("select marital_status, count(*) from df group by marital_status")
sqldf("select race, count(*) from df group by race")
sqldf("select sex, count(*) from df group by sex")
sqldf("select native_country, count(*) from df group by native_country")

library(gmodels)
CrossTable(x = df$workclass, y = df$salary_class, prop.t=FALSE, expected=TRUE, chisq =TRUE)
CrossTable(x = df$marital_status, y = df$salary_class, prop.t=FALSE, expected=TRUE, chisq =TRUE)
CrossTable(x = df$race, y = df$salary_class, prop.t=FALSE, expected=TRUE, chisq =TRUE)
CrossTable(x = df$sex, y = df$salary_class, prop.t=FALSE, expected=TRUE, chisq =TRUE)
CrossTable(x = df$native_country, y = df$salary_class, prop.t=FALSE, expected=TRUE, chisq =TRUE)
