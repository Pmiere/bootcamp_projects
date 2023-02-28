##correlation
cor(mtcars$hp,mtcars$mpg)
cor(mtcars$wt,mtcars$mpg)

plot(mtcars$hp,mtcars$mpg, pch = 16)
plot(mtcars$hp,mtcars$wt, pch = 16)

cor(mtcars[ , c("mpg","wt","hp")])

##dplyr(tidyverse)
library(dplyr)
cormat <- mtcars %>%
  select(mpg,wt,hp,am) %>%
  cor()

cor(mtcars$hp,mtcars$mpg)
cor.test(mtcars$hp,mtcars$mpg) ##ทดสอบดูค่า correlationT.test, sig test



##linear regression
##mpg = f(hp)
lmfit <- lm(mpg ~ hp, data = mtcars)
summary(lmfit)

##Prediction
lmfit$coefficients[[1]] + lmfit$coefficients[[2]]*200
#intercept                 slope                  hp=200

new_cars <- data.frame (
  hp = c(250,320,400,410,450) #Predict450 hp เกินค่าเดิมที่มีmax=335
)

## Predict()
new_cars$mpg_pred <- predict(lmfit,newdata = new_cars) ##เอาไปต่อเป็นคอลัมน์ใหม่ด้านขวา
new_cars$hp_pred <- NULL
new_cars #Predict450 hp เกินค่าเดิมที่มี max=335  mpg_predติดลบ

summary(mtcars$hp)

##Root mean square error (rmse)
## multiple linear regression
##mpg = f(hp.wt,am)
##mpg + intercept + b0*hp +b1*wt +b3*am

lmfit2 <- lm(mpg ~ hp +wt +am, data = mtcars)

coefs <- coef(lmfit2)
coefs[[1]] + coefs[[2]]*200 + coefs[[3]]*3.5 + coefs[[4]]*1

##build_full model
lmfitfull <- lm(mpg ~ ., data = mtcars) #- gear เติมหลัง . เพื่อเอาคอลัมน์นี้ออกได้

mtcars$prediected <- predict(lmfitfull)

##train RMSE
squared_error <- (mtcars$mpg - mtcars$prediected) **2 
(rmse <- sqrt(mean(squared_error))) ##ค่าที่น่าจะทำนายผิดโดยประมาณ

##split data
set.seed(42)
n <- nrow(mtcars) ##นับแถวข้อมูล
id <- sample(1:n, size = n*0.8) ##เอามา 80%
train_data <- mtcars[id,] ##80%เอามาเทรน
test_data <- mtcars[-id,] ##ที่เหลือเอามาเทส

##train model
model1 <- lm(mpg ~ hp +wt +am, data = train_data)
p_train <- predict(model1)
error <- train_data$mpg - p_train
rmse <- sqrt(mean(error**2))


##test model
p_test <- predict(model1, newdata = test_data) ##ทำนายโดยใช้ข้อมูลที่แยกไว้
error_test <- test_data$mpg - p_test
rmse_test <- sqrt(mean(error_test**2))

##print result
cat("rmse train : ",rmse,
    "\nrmse test : ",rmse_test)
