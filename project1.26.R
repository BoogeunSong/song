####################################
###########유비온3회차##############
####################################
setwd("C:/rproject/ubion")
getwd()

df<-read.csv("Ashopping.csv")
head(df)
str(df)
summary(df)
hist(df$평균.구매주기)
df$평균.구매주기<-log1p(df$평균.구매주기)
hist(df$평균.구매주기)

#install.packages("caret",dependencies = T, type="binary")
library(caret)
#install.packages("vctrs", type="binary")
#library(vctrs)
#install.packages("mltools",dependencies = T, type="binary")
library(mltools)
#install.packages("data.table",dependencies = T, type="binary")
library(data.table)
.libPaths()

colnames(df)
num<-c("총매출액","X1회.평균매출액","할인권.사용.횟수",
      "총.할인.금액","구매카테고리수","Recency","Frequency",
      "Monetary")
cg<-c("구매금액대","고객등급","구매유형","클레임접수여부",
      "거주지역","성별","고객.나이대","할인민감여부")

#변수지정
X<-df[df$이탈여부==0, c(num,cg)]
Y<-data.frame(df[df$이탈여부==0,"평균.구매주기"])
X[,cg]<-lapply(X[,cg],factor)
str(X)
str(Y)
#length() 
#df_exit<-df[df$이탈여부==1,]
#length(df_exit$이탈여부) #이탈여부 1이 몇개인지 찾는 코드


#파티션 분할

partition <- createDataPartition(X$총매출액, p=0.7, list=F)
X_train <- X[partition,]
X_test <- X[-partition,]
Y_train <- Y[partition,]
Y_test <- Y[-partition,]
str(X_train)
str(X_test)
str(Y_train)
str(Y_test)
colnames(df)
Y_train<-data.frame(Y_train)
Y_test<-data.frame(Y_test)
colnames(Y_train)<-"평균.구매주기"
colnames(Y_test)<-"평균.구매주기"
colSums(is.na(Y_train))
colSums(is.na(Y_test))
colSums(is.na(X_train))
colSums(is.na(X_test))


#표준화 / 원핫인코딩  
#margin 1이면 row로 뽑고 2면 열별로 뽑아줘라
#apply는 fun을 적용해주는 함수
X_train_num <- apply(X_train[,num], MARGIN = 2, FUN = scale)  
X_test_num <- apply(X_test[,num], MARGIN = 2, FUN = scale)
X_train_num <-data.frame(X_train_num)
X_test_num <-data.frame(X_test_num)
help(apply)
X_train_cg <- one_hot(as.data.table(X_train[,cg]))
X_test_cg <- one_hot(as.data.table(X_test[,cg]))
help(one_hot)
X_train <-cbind(X_train_num,X_train_cg)
X_test <-cbind(X_test_num,X_test_cg)
str(X_train)
str(X_test)

#모형 구축(학습)
#.은 X의 모든 변수를 넣겠다
#평균구매주기는 Y 
# ***,. 이 붙어있으면 의미가 있는 열임
#베타가 0이면 영향이 있다 대립가설 채택
#회귀분석 일반
reg <- lm(Y_train$평균.구매주기~ . , data=X_train)
summary(reg)

#step
#AIC를 기준으로 제일 좋은 모델을 뽑아줌(AIC가 높으면 좋은 모델임)
reg1 <- step(lm(Y_train$평균.구매주기~ . , data=X_train))
summary(reg1)
help(step)

#모형 평가
result <- predict(reg1, newdata = X_test)
cor(result, Y_test$평균.구매주기)   #상관관계 분석
result_test <- cbind(result,Y_test)
result_test
#install.packages("forecast",dependencies = T, type="binary")
library(forecast)
accuracy(result, Y_test$평균.구매주기)
reg1$coefficients

# y= 5.00046001+(-0.18559776*총매출액)+(0.38327111*X1회.평균매출액)+


##############################수치예측####################################
#Ridge regression
#install.packages("glmnet",dependencies = T, type="binary")
library(glmnet)
help("glmnet")
#릿지는 alpha가 0 라쏘는 alpha가 1 elastic은 alpha가 0.5
ridge <- glmnet(X_train, Y_train$평균.구매주기, alpha=0,
                lambda=0)
summary(ridge)
ridge$a0
ridge$beta
ridge$lambda
#모형 평가
ridge_result <- predict(ridge, newx= as.matrix((X_test)))
ridge_result
cor(Y_test$평균.구매주기, ridge_result)
accuracy(ridge_result[,],Y_test$평균.구매주기)

#lasso 
lasso <- glmnet(X_train, Y_train$평균.구매주기, alpha=1,
                lambda=0)
summary(lasso)
lasso_result <- predict(lasso, s=0, newx= as.matrix((X_test)))
lasso_result
cor(Y_test$평균.구매주기, lasso_result)
accuracy(lasso_result[,],Y_test$평균.구매주기)

#elastic
elastic <- glmnet(X_train, Y_train$평균.구매주기, alpha=0.5,
                lambda=0)
summary(elastic)
elastic$beta
elastic_result <- predict(elastic, s=0, newx= as.matrix((X_test)))
elastic_result
cor(Y_test$평균.구매주기, elastic_result)
accuracy(elastic_result[,],Y_test$평균.구매주기)

#k-최근접 이웃(knn)
train <- cbind(X_train,Y_train)
colnames(train)
#모형구축
knn <- caret::train(평균.구매주기~.,
                    data=train,
                    method = "knn",
                    tuneGrid=expand.grid(k=1:15))
summary(knn)
knn$results
#예측
knn_pred <- predict(knn,X_test)
knn_pred
#평가
cor(knn_pred, Y_test$평균.구매주기)
accuracy(knn_pred,Y_test$평균.구매주기)



#의사결정나무
train <- cbind(X_train,Y_train)
colnames(train)
#모형구축
tree <- caret::train(평균.구매주기~.,
                    data=train,
                    method = "rpart")
summary(tree)
tree$results
#예측
tree_pred <- predict(tree,X_test)
tree_pred
#평가
cor(tree_pred, Y_test$평균.구매주기)
accuracy(tree_pred,Y_test$평균.구매주기)
varImp(tree) #영향을 많이 미치는 변수 출력
#시각화
#install.packages("rpart.plot",dependencies = T, type="binary")
library(rpart.plot)
prp(tree$finalModel)
#write.csv(tree_pred,"pred.csv")  #만든 dataframe 엑셀파일로 저장~


#svm linear
#모형구축
svm <- caret::train(평균.구매주기~.,
                     data=train,
                     method = "svmLinear")
summary(svm)
#예측
svm_pred <- predict(svm,X_test)
svm_pred
#평가
cor(svm_pred, Y_test$평균.구매주기)
accuracy(svm_pred,Y_test$평균.구매주기)


#나이브 베이즈
nb <- caret::train(평균.구매주기~.,
                    data=train,
                    method = "bridge")
summary(nb)
#예측
nb_pred <- predict(nb,X_test)
nb_pred
#평가
cor(nb_pred, Y_test$평균.구매주기)
accuracy(nb_pred,Y_test$평균.구매주기)


#랜덤포레스트
rf <- caret::train(평균.구매주기~.,
                   data=train,
                   method = "cforest")
summary(rf)
#예측
rf_pred <- predict(rf,X_test)
rf_pred
#평가
cor(rf_pred, Y_test$평균.구매주기)
accuracy(rf_pred,Y_test$평균.구매주기)
varImp(rf)


##########################분류(classification)###########################
#로지스틱 회귀
str(df)
num<-c("총매출액","X1회.평균매출액","할인권.사용.횟수",
       "총.할인.금액","구매카테고리수","Recency","Frequency",
       "Monetary")
cg<-c("구매금액대","고객등급","구매유형","클레임접수여부",
      "거주지역","성별","고객.나이대","할인민감여부")

#변수지정
X<-df[, c(num,cg)]
Y <- data.frame(as.factor(df$이탈여부))
X[,cg]<-lapply(X[,cg],factor)
#파티션 분할
partition <- createDataPartition(X$총매출액, p=0.7, list=F)
X_train <- X[partition,]
X_test <- X[-partition,]
Y_train <- Y[partition,]
Y_test <- Y[-partition,]
str(X_train)
str(X_test)
str(Y_train)
str(Y_test)
#표준화 / 원핫인코딩
X_train_num <- apply(X_train[,num], MARGIN = 2, FUN = scale)  
X_test_num <- apply(X_test[,num], MARGIN = 2, FUN = scale)
X_train_num <-data.frame(X_train_num)
X_test_num <-data.frame(X_test_num)
help(apply)
X_train_cg <- one_hot(as.data.table(X_train[,cg]))
X_test_cg <- one_hot(as.data.table(X_test[,cg]))
#help(one_hot)
X_train <-cbind(X_train_num,X_train_cg)
X_test <-cbind(X_test_num,X_test_cg)
str(X_train)
str(X_test)

#오버샘플링
df2<-upSample(X_train,Y_train)
X_train <-subset(df2,select = -c(Class))
Y_train <-df2["Class"]  
str(X_train)
str(Y_train)

#로지스틱 회귀 모형 학습
logit <-glm(Y_train$Class~. , data = X_train, family = binomial)
help(glm)
summary(logit)
logit_step <- step(logit)
summary(logit_step)

#Y 예측
logit_pred <- predict(logit_step,X_test)
logit_pred <- ifelse(logit_pred>0.5,1,0)
logit_pred <-as.factor(logit_pred)
Y_test <-as.factor(Y_test)
caret::confusionMatrix(logit_pred,Y_test)
logit_step$coefficients

#다항로지스틱
#library(nnet)
#multi <-multinom(formula,data) #y가 0,1이아니고 1,2,3,4 다수의 집단이 있는경우 multinom사용

#knn
train_logit <- cbind(X_train, Y_train)
str(train_logit)
knn_logit <- caret::train(Class~.,
                          data = train_logit,
                          method ="knn",
                          tuneGrid = expand.grid(k=1:30))     
summary(knn_logit)
knn_logit$results

#예측
knn_logit_result <- predict(knn_logit,X_test)
knn_logit_result
#confusionMatrix:혼동행렬표 #Sensitivity,Specificity 정도 보면된다
caret::confusionMatrix(knn_logit_result, Y_test) 


#의사결정나무
tree_logit <- caret::train(Class~.,
                          data = train_logit,
                          method ="rpart")     
summary(tree_logit)
tree_logit$results
#예측
tree_logit_result <- predict(tree_logit,X_test)
tree_logit_result
caret::confusionMatrix(tree_logit_result, Y_test) 
prp(tree_logit$finalModel)


#SVM
svm_logit <- caret::train(Class~.,
                           data = train_logit,
                           method ="svmLinear")     
summary(svm_logit)
svm_logit$results
#예측 및 평가 시각화
svm_logit_result <- predict(svm_logit,X_test)
svm_logit_result
caret::confusionMatrix(svm_logit_result, Y_test) 


#나이브 베이즈
nb_logit <- caret::train(Class~.,
                          data = train_logit,
                          method ="naive_bayes")     
summary(nb_logit)
nb_logit$results
#예측 및 평가 시각화
nb_logit_result <- predict(nb_logit,X_test)
nb_logit_result
caret::confusionMatrix(nb_logit_result, Y_test) 


#랜덤포레스트
rf_logit <- caret::train(Class~.,
                         data = train_logit,
                         method ="cforest")     
summary(rf_logit)
rf_logit$results
#예측 및 평가 시각화
rf_logit_result <- predict(rf_logit,X_test)
rf_logit_result
caret::confusionMatrix(rf_logit_result, Y_test,mode="everything") 










