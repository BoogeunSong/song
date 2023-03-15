########################################
##############�����4��#################
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

#�ΰ��Ű��
colnames(df)
num <- c("�Ѹ����","X1ȸ.��ո����","�ŷ��Ⱓ",
        "���.�����ֱ�") 
cg <- c("���űݾ״�","�������")
X <- df[,c(num,cg)]
Y <- df$���ιΰ�����
Y <- data.frame(as.factor(Y))
X$���űݾ״� <- as.factor(X$���űݾ״�)
X$������� <- as.factor(X$�������)
str(X)
str(Y)

#������ ����
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

#ǥ��ȭ /�������ڵ�
#ǥ��ȭ
X_train_num <- apply(X_train[,num], MARGIN = 2, FUN = scale)
X_trian_num <- data.frame(X_train_num)
X_test_num <- apply(X_test[,num], MARGIN = 2, FUN = scale)
X_test_num <- data.frame(X_test_num)


#����
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


#�������ø� :�з������� ��쿡�� ���
df2 <- upSample(X_train, Y_train)
str(df2)

#���� �н�
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

#####��ġ����#####
str(df)

colnames(df)
num <- c("�湮��","��.����.�ݾ�","�ŷ��Ⱓ","���.�����ֱ�")
cg <- c("�������","���ιΰ�����")
X <- df[,c(num,cg)]
Y <- df$X1ȸ.��ո����
hist(Y)
Y <- data.frame(log1p(Y))
hist(Y$log1p.Y.)
X$������� <- as.factor(X$�������)
X$���ιΰ�����<-as.factor(X$���ιΰ�����)
str(X)
str(Y)


#������ ����
partition <- createDataPartition(Y$log1p.Y., p=0.7, list = F)
X_train <- X[partition,]
X_test <- X[-partition,]
Y_train <- Y[partition,]
Y_test <- Y[-partition,]
str(X_train)
str(X_test)
str(Y_train)
str(Y_test)


#ǥ��ȭ/�������ڵ�
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


#####���� �н�#####
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


####�ӻ��#####
####�з�####
colnames(df)
num <- c("�Ѹ����","X1ȸ.��ո����","�ŷ��Ⱓ",
         "���.�����ֱ�") 
cg <- c("���űݾ״�","�������")
X <- df[,c(num,cg)]
Y <- df$���ιΰ�����
Y <- data.frame(as.factor(Y))
X$���űݾ״� <- as.factor(X$���űݾ״�)
X$������� <- as.factor(X$�������)
str(X)
str(Y)

#������ ����
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

#ǥ��ȭ /�������ڵ�
#ǥ��ȭ
X_train_num <- apply(X_train[,num], MARGIN = 2, FUN = scale)
X_trian_num <- data.frame(X_train_num)
X_test_num <- apply(X_test[,num], MARGIN = 2, FUN = scale)
X_test_num <- data.frame(X_test_num)


#����
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


#�������ø� :�з������� ��쿡�� ���
df2 <- upSample(X_train, Y_train)
str(df2)

X_train <- subset(df2, select =  -c(Class))
Y_train <- df2$Class
train_data <- cbind(X_train, Y_train)

#�����н� : adabag(������)

adabag_model <- caret::train(Y_train~., data=train_data, method="AdaBag")
adabag_model

#��������
Y_adabag_pred <- predict(adabag_model, X_test)
caret::confusionMatrix(Y_adabag_pred, Y_test, mode="everything")


#����������Ʈ, �ν���(�׸����Ʈ, adaboost)

#����������ƮƮ

rf_model <- caret::train(Y_train~., data=train_data, method="cforest")
rf_model

#��������
Y_rf_pred <- predict(rf_model, X_test)
caret::confusionMatrix(Y_rf_pred, Y_test, mode="everything")


#roc� �̴� �ڵ��ӵ�
#library(pROC)
#Y_pred_test_rf <-data.frame(cbind(Y_test,Y_rf_pred))
#roc_rf <- roc(Y_pred_test_rf$Y_test, Y_pred_test_rf$Y_rf_pred)
#plot(roc_rf)


#adaboost
ab_model <- caret::train(Y_train~., data=train_data, method="AdaBoost.M1")
ab_model

#��������
Y_ab_pred <- predict(ab_model, X_test)
caret::confusionMatrix(Y_ab_pred, Y_test, mode="everything")

#gbm(gradient boost model)
gbm_model <- caret::train(Y_train~., data=train_data, method="gbm")
gbm_model

#��������
Y_gbm_pred <- predict(gbm_model, X_test)
caret::confusionMatrix(Y_gbm_pred, Y_test, mode="everything")



##��ġ������
str(df)
colnames(df)
num <- c("�湮��","��.����.�ݾ�","�ŷ��Ⱓ","���.�����ֱ�")
cg <- c("�������","���ιΰ�����")
X <- df[,c(num,cg)]
Y <- df$X1ȸ.��ո����
hist(Y)
Y <- data.frame(log1p(Y))
hist(Y$log1p.Y.)
X$������� <- as.factor(X$�������)
X$���ιΰ�����<-as.factor(X$���ιΰ�����)
str(X)
str(Y)


#������ ����
partition <- createDataPartition(Y$log1p.Y., p=0.7, list = F)
X_train <- X[partition,]
X_test <- X[-partition,]
Y_train <- Y[partition,]
Y_test <- Y[-partition,]
str(X_train)
str(X_test)
str(Y_train)
str(Y_test)


#ǥ��ȭ/�������ڵ�
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


#####���� �н�#####
train_data <- cbind(X_train, Y_train)

##���(treebag:adabag ����, cforest:����������Ʈ), 
##�ν���(gbm:�׸����ν�Ʈ,glmboost,M1:ada�ν�Ʈ)
##method = "�����ֱ�"

#######treebag###########
treebag_reg <- caret::train(Y_train~., data=train_data, method="treebag")
Y_treebag_pred <- predict(treebag_reg, X_test)
cor(Y_treebag_pred, Y_test)
accuracy(Y_treebag_pred, Y_test)

########����������Ʈ############
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



