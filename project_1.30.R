########################################
##############유비온4차#################
########################################

getwd()
setwd("C:/rproject/ubion")
getwd()

library(caret)
library(mltools)
library(data.table)
#install.packages("RSNNS",dependencies = T, Type = "binary")
library(RSNNS)

df <- read.csv("Ashopping.csv")
str(df)
summary(df)
#.libPaths()

#인공신경망
colnames(df)
num <- c("총매출액","X1회.평균매출액","거래기간",
        "평균.구매주기") 
cg <- c("구매금액대","고객등급")
X <- df[,c(num,cg)]
Y <- df$할인민감여부
Y <- data.frame(as.factor(Y))
X$구매금액대 <- as.factor(X$구매금액대)
X$고객등급 <- as.factor(X$고객등급)
str(X)
str(Y)

#데이터 분할
partition <- createDataPartition(Y$as.factor.Y., p=0.7, list =F)
X_train <- X[partition,] 
X_test <- X[-partition,]
Y_train <- Y[partition,]
Y_test <- Y[-partition,]
str(X_train)
str(X_test)
str(Y_train)
str(Y_test)
length(Y_train)
length(Y_test)
colSums(is.na(X_train))
colSums(is.na(X_test))
#Y_train <- data.frame(Y_train)
#Y_test <- data.frame(Y_test)
colSums(is.na(Y_train))
colSums(is.na(Y_test))

#표준화 /원핫인코딩
#표준화
X_train_num <- apply(X_train[,num], MARGIN = 2, FUN = scale)
X_trian_num <- data.frame(X_train_num)
X_test_num <- apply(X_test[,num], MARGIN = 2, FUN = scale)
X_test_num <- data.frame(X_test_num)


#원핫
X_train_cg <-one_hot(as.data.table(X_train[,cg]))
X_test_cg <-one_hot(as.data.table(X_test[,cg]))
??one_hot

str(X_train_num)
str(X_train_cg)
str(X_test_num)
str(X_test_cg)

X_train <-cbind(X_train_num, X_train_cg)
X_test <- cbind(X_test_num, X_test_cg)
str(X_train)
str(X_test)


#오버샘플링 :분류모형일 경우에만 사용
df2 <- upSample(X_train, Y_train)
str(df2)

#모형 학습
X_train <- subset(df2, select = -c(Class))
str(X_train)
Y_train <- df2$Class
train_data <-cbind(X_train, Y_train)
str(train_data)
mlp_model <- caret::train(Y_train ~.,data = train_data,method = "mlp")
mlp_model
mlp_model$finalModel

mlp_pred <- predict(mlp_model, X_test)
mlp_pred
Y_test <- as.factor(Y_test)
caret::confusionMatrix(mlp_pred,Y_test, mode="everything")

#####수치예측#####
str(df)

colnames(df)
num <- c("방문빈도","총.할인.금액","거래기간","평균.구매주기")
cg <- c("고객등급","할인민감여부")
X <- df[,c(num,cg)]
Y <- df$X1회.평균매출액
hist(Y)
Y <- data.frame(log1p(Y))
hist(Y$log1p.Y.)
X$고객등급 <- as.factor(X$고객등급)
X$할인민감여부<-as.factor(X$할인민감여부)
str(X)
str(Y)


#데이터 분할
partition <- createDataPartition(Y$log1p.Y., p=0.7, list = F)
X_train <- X[partition,]
X_test <- X[-partition,]
Y_train <- Y[partition,]
Y_test <- Y[-partition,]
str(X_train)
str(X_test)
str(Y_train)
str(Y_test)


#표준화/원핫인코딩
X_train_num <- apply(X_train[, num], MARGIN = 2, FUN = scale)
X_train_num <- data.frame(X_train_num)
X_test_num <- apply(X_test[, num], MARGIN = 2, FUN = scale)
X_test_num <- data.frame(X_test_num)

X_train_cg <- one_hot(as.data.table(X_train[, cg]))
X_test_cg <- one_hot(as.data.table(X_test[, cg]))

X_train <- cbind(X_train_num, X_train_cg)
X_test <- cbind(X_test_num, X_test_cg)
str(X_train)
str(X_test)


#####모형 학습#####
train_data <- cbind(X_train, Y_train)
mlp_reg <- caret::train(Y_train~., data=train_data, method="mlp")
mlp_reg$finalModel
Y_mlp_pred <- predict(mlp_reg, X_test)
head(Y_mlp_pred)
cor(Y_mlp_pred, Y_test)
library(forecast)
accuracy(Y_mlp_pred, Y_test)
cor(Y_mlp_pred, Y_test)
Y_pred_test<-cbind(Y_mlp_pred, Y_test)
head(Y_pred_test,20)


####앙상블#####
####분류####
colnames(df)
num <- c("총매출액","X1회.평균매출액","거래기간",
         "평균.구매주기") 
cg <- c("구매금액대","고객등급")
X <- df[,c(num,cg)]
Y <- df$할인민감여부
Y <- data.frame(as.factor(Y))
X$구매금액대 <- as.factor(X$구매금액대)
X$고객등급 <- as.factor(X$고객등급)
str(X)
str(Y)

#데이터 분할
partition <- createDataPartition(Y$as.factor.Y., p=0.7, list =F)
X_train <- X[partition,] 
X_test <- X[-partition,]
Y_train <- Y[partition,]
Y_test <- Y[-partition,]
str(X_train)
str(X_test)
str(Y_train)
str(Y_test)
#length(Y_train)
#length(Y_test)
#colSums(is.na(X_train))
#colSums(is.na(X_test))
#Y_train <- data.frame(Y_train)
#Y_test <- data.frame(Y_test)
#colSums(is.na(Y_train))
#colSums(is.na(Y_test))

#표준화 /원핫인코딩
#표준화
X_train_num <- apply(X_train[,num], MARGIN = 2, FUN = scale)
X_trian_num <- data.frame(X_train_num)
X_test_num <- apply(X_test[,num], MARGIN = 2, FUN = scale)
X_test_num <- data.frame(X_test_num)


#원핫
X_train_cg <-one_hot(as.data.table(X_train[,cg]))
X_test_cg <-one_hot(as.data.table(X_test[,cg]))

str(X_train_num)
str(X_train_cg)
str(X_test_num)
str(X_test_cg)

X_train <-cbind(X_train_num, X_train_cg)
X_test <- cbind(X_test_num, X_test_cg)
str(X_train)
str(X_test)


#오버샘플링 :분류모형일 경우에만 사용
df2 <- upSample(X_train, Y_train)
str(df2)

X_train <- subset(df2, select =  -c(Class))
Y_train <- df2$Class
train_data <- cbind(X_train, Y_train)

#모형학습 : adabag(배깅모형)

adabag_model <- caret::train(Y_train~., data=train_data, method="AdaBag")
adabag_model

#모형예측
Y_adabag_pred <- predict(adabag_model, X_test)
caret::confusionMatrix(Y_adabag_pred, Y_test, mode="everything")


#랜덤포레스트, 부스팅(그리디언트, adaboost)

#랜덤포레스트트

rf_model <- caret::train(Y_train~., data=train_data, method="cforest")
rf_model

#모형예측
Y_rf_pred <- predict(rf_model, X_test)
caret::confusionMatrix(Y_rf_pred, Y_test, mode="everything")


#roc곡선 뽑는 코드임돠
#library(pROC)
#Y_pred_test_rf <-data.frame(cbind(Y_test,Y_rf_pred))
#roc_rf <- roc(Y_pred_test_rf$Y_test, Y_pred_test_rf$Y_rf_pred)
#plot(roc_rf)


#adaboost
ab_model <- caret::train(Y_train~., data=train_data, method="AdaBoost.M1")
ab_model

#모형예측
Y_ab_pred <- predict(ab_model, X_test)
caret::confusionMatrix(Y_ab_pred, Y_test, mode="everything")

#gbm(gradient boost model)
gbm_model <- caret::train(Y_train~., data=train_data, method="gbm")
gbm_model

#모형예측
Y_gbm_pred <- predict(gbm_model, X_test)
caret::confusionMatrix(Y_gbm_pred, Y_test, mode="everything")



##수치예측측
str(df)
colnames(df)
num <- c("방문빈도","총.할인.금액","거래기간","평균.구매주기")
cg <- c("고객등급","할인민감여부")
X <- df[,c(num,cg)]
Y <- df$X1회.평균매출액
hist(Y)
Y <- data.frame(log1p(Y))
hist(Y$log1p.Y.)
X$고객등급 <- as.factor(X$고객등급)
X$할인민감여부<-as.factor(X$할인민감여부)
str(X)
str(Y)


#데이터 분할
partition <- createDataPartition(Y$log1p.Y., p=0.7, list = F)
X_train <- X[partition,]
X_test <- X[-partition,]
Y_train <- Y[partition,]
Y_test <- Y[-partition,]
str(X_train)
str(X_test)
str(Y_train)
str(Y_test)


#표준화/원핫인코딩
X_train_num <- apply(X_train[, num], MARGIN = 2, FUN = scale)
X_train_num <- data.frame(X_train_num)
X_test_num <- apply(X_test[, num], MARGIN = 2, FUN = scale)
X_test_num <- data.frame(X_test_num)

X_train_cg <- one_hot(as.data.table(X_train[, cg]))
X_test_cg <- one_hot(as.data.table(X_test[, cg]))

X_train <- cbind(X_train_num, X_train_cg)
X_test <- cbind(X_test_num, X_test_cg)
str(X_train)
str(X_test)


#####모형 학습#####
train_data <- cbind(X_train, Y_train)

##배깅(treebag:adabag 일종, cforest:랜덤포레스트), 
##부스팅(gbm:그리디언부스트,glmboost,M1:ada부스트)
##method = "모형넣기"

#######treebag###########
treebag_reg <- caret::train(Y_train~., data=train_data, method="treebag")
Y_treebag_pred <- predict(treebag_reg, X_test)
cor(Y_treebag_pred, Y_test)
accuracy(Y_treebag_pred, Y_test)

########랜던포레스트############
rf_reg <- caret::train(Y_train~., data=train_data, method="cforest")
Y_rf_pred <- predict(rf_reg, X_test)
cor(Y_rf_pred, Y_test)
accuracy(Y_rf_pred, Y_test)


#########gbm
gbm_reg <- caret::train(Y_train~., data=train_data, method="gbm")
Y_gbm_pred <- predict(gbm_reg, X_test)
cor(Y_gbm_pred, Y_test)
accuracy(Y_gbm_pred, Y_test)


#######glmboost
glmboost_reg <- caret::train(Y_train~., data=train_data, method="glmboost")
Y_glmboost_pred <- predict(glmboost_reg, X_test)
cor(Y_glmboost_pred, Y_test)
accuracy(Y_glmboost_pred, Y_test)




