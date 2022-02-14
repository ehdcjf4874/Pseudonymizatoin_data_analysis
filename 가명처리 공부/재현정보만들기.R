# ?۾? ?????丮 ??��
setwd("C:/Users/ehdcj/OneDrive/바탕 화면")

# ?????? ?б?
german_credit <- read.csv("german credit data.csv")

str(german_credit)


# ???? ��?ǿ? ?°? ???? Ÿ?? ????
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


res <- cor(german_credit[sapply(german_credit, is.numeric)])
round(res,2)
res <- cor(synData[sapply(synData, is.numeric)])
round(res,2)
