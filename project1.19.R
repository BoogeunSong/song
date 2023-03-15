##############################
#######����� R 2ȸ��#########
##############################

#NA, NULL
a <- NULL
is.null(a)
typeof(a)

b <- NA
is.na(b)
typeof(b)

c <- c(1, 2, 3, NA, NULL)
is.null(c)
is.na(c)

c <-as.data.frame(c)
is.na(c$c) ##c$c �� c��� df�ȿ� c��� ������ �ִ�. ��� ��

#NAN, Inf
d <-10/0
d #inf(���Ѵ�-infinity)
e <- -10/0
e
f <- 0/0
f

#������Ÿ�� ��ȯ
data <- c(1, 2, 3)
typeof(data)
d1 <- as.character(data)
typeof(d1) ##����
d2 <- as.numeric(data)
typeof(d2) ##����
d3 <- as.factor(data)
typeof(d3)##����
d4 <- as.matrix(data)
typeof(d4)##���
d5 <- as.array(data)
typeof(d5)##�迭
d6 <- as.data.frame(data)
typeof(d6)##������������

#dplyr
#.libpaths()
#install.packages("dplyr",dependencies = T)
library(dplyr)
library(rlang)
#install.packages("rlang", dependencise = T, type = "binary")

#������ �����ڸ� ���� ����
## - %>%  : ctr +shift +M
## dot�� ����� ���� 
head(iris,10)
str(iris) ##summary���� �����غ�����!!
summary(iris) ##�⺻�� ��谪 
iris <- data.frame(iris)

##iris��� ��������
iris %>% filter(Sepal.Length > 6)
sepallen_over6 <- data.frame(iris %>% filter(Sepal.Length > 6))
sepallen_over6 %>% select(Sepal.Length, Species)
iris %>% filter(Sepal.Length > 6) %>% select(Sepal.Length, Species)
iris %>%  arrange(Sepal.Length) ##���� // ���������� ����
iris %>% filter(Sepal.Length > 6) %>% select(Sepal.Length, Species) %>% arrange(Sepal.Length)
help(dplyr)
??dplyr
iris %>% mutate(Species_n = factor(Species)) %>% group_by(Species_n) %>% summarise(n())


getwd()
setwd("C:/rproject/ubion")
df<-read.csv("Ashopping2.csv",encoding = "cp949")#UTF-8 #fileEncoing ="ecu-kr
head(df)
str(df)
summary(df)
df$ ��Ż����<-as.factor(df$��Ż����)
df$��_�����<-as.numeric(df$��_�����)

#���Ϻҷ�����

#install.packages("readxl")
library(readxl)

head(df)
str(df)
summary(df)

#����ġ
is.na(df)
complete.cases(df)  #�Ųٷ� ����� (Flase�� true��)
colSums(is.na(df))

a<-c(NA,1,2,3,4)
a<-data.frame(a)
colSums(is.na(a)) #na�� true�� ������ �˷���

#����ġ ����
df_del_row<- na.omit(df)    #omit ����
summary(df_del_row)
colSums(is.na(df_del_row))

#����ġ ��ü
df$��ǰ_����_��ġ<-ifelse(is.na(df$��ǰ_����_��ġ),
                    mean(df$��ǰ_����_��ġ,na.rm=T),
                    df$��ǰ_����_��ġ)
colSums(is.na(df))
df$��ǰ_����_ǥ��<-ifelse(is.na(df$��ǰ_����_ǥ��),
                    mean(df$��ǰ_����_ǥ��,na.rm=T),
                    df$��ǰ_����_ǥ��)
colSums(is.na(df))

#�̻�ġ
boxplot(df$��_�����)
hist(df$��_�����)

iqr <-IQR(df$��_�����)

df_iqr<-df[(df$��_�����<median(df$��_�����)+iqr*2)
           &(df$��_�����>median(df$��_�����)-iqr*2),]
hist(df_iqr$��_�����)
hist(df$��_�����)

df$��_�����<-log1p(df$��_�����)
hist(df$��_�����)

#������ ����, ����
head(df)
under_500<-df[df$����ID<=500,]
over_500 <- df[df$����ID>500,]
df_1000 <- rbind(under_500,over_500)
str(df_1000)

#index
df[,c("����ID","��Ż����")]
df[150:200,c("����ID","��Ż����")]

#merge
df1<-df[,c("����ID","��Ż����")]
df2<-df[,c("����ID","��_�����")]
head(df1)
head(df2)
df_merge <- merge(df1,df2,all=T)
help(merge)
head(df_merge)

#������
summary(df)
colnames(df)
df1<-df[,c("����","���α�_���.Ƚ��")]
str(df1)
df1$���� <-as.factor(df1$����)
print(df1 %>% group_by(����) %>% summarise(mean(���α�_���.Ƚ��)))
print(df1 %>% group_by(����) %>% summarise(var(���α�_���.Ƚ��)))
print(df1 %>% group_by(����) %>% summarise(sd(���α�_���.Ƚ��)))

df$�������<- as.factor(df$�������)
print(df %>% group_by(�������) %>% summarise(mean(���α�_���.Ƚ��)))
## %>%  ->dplyr ��Ű��

#t-test
##���Լ�����
#shapiro-wilk
shapiro.test(df$��_�����)
hist(df$��_�����)
ks.test(df$��_�����,pnorm) #���Ժ��� �ƴѰ� Ȯ������

shapiro.test(over_500$��_�����)
shapiro.test(under_500$��_�����)

#������� �м� cor(df$��_�����, df$���α�_���.Ƚ��)
#������ ����
df_chisq <- table(df$Ŭ������������, df$��Ż����)
chisq.test(df_chisq)

#��л� ����
#install.packages("lawstat")
#help("levene.test")
library(lawstat)
df$����<-as.factor(df$����)
df$����<-as.integer(df$����)
levene.test(df$��_�����,df$����)
bartlett.test(df$��_�����,df$����)
levene.test(df$�湮��,df$Ŭ������������)
bartlett.test(df$�湮��,df$Ŭ������������)

df_m <- df[df$���� ==1,]  
df_w <- df[df$���� ==2,]  
par(mfrow=c(1,2))
hist(df_m$��_�����)
hist(df_w$��_�����)

#t-test
t.test(df_m$��_�����, df_w$��_�����,var.equal =F)

#ANOVA
anova <- aov(��_����� ~��������,data=df)
summary(anova)
print(df %>% group_by(��������) %>% summarise(mean(��_�����)))







