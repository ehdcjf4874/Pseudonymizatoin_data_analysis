head(cars)
plot(cars$speed, cars$dist)
cars$dist1 = as.integer(cars$dist/10)*10 # 첫째자리 수가 Round처리됨
cor(cars$speed, cars$dist) #상관계수
cor(cars$speed, cars$dist1) #상관계수가 떨어짐 -> 라운딩 처리로 인한 정보손실

