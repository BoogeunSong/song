##############################
#######유비온 R 2회차#########
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
is.na(c$c) ##c$c 는 c라는 df안에 c라는 변수가 있다. 라는 뜻

#NAN, Inf
d <-10/0
d #inf(무한대-infinity)
e <- -10/0
e
f <- 0/0
f

#데이터타입 변환
data <- c(1, 2, 3)
typeof(data)
d1 <- as.character(data)
typeof(d1) ##글자
d2 <- as.numeric(data)
typeof(d2) ##숫자
d3 <- as.factor(data)
typeof(d3)##집단
d4 <- as.matrix(data)
typeof(d4)##행렬
d5 <- as.array(data)
typeof(d5)##배열
d6 <- as.data.frame(data)
typeof(d6)##데이터프레임

#dplyr
#.libpaths()
#install.packages("dplyr",dependencies = T)
library(dplyr)
library(rlang)
#install.packages("rlang", dependencise = T, type = "binary")

#파이프 연산자를 통한 정제
## - %>%  : ctr +shift +M
## dot과 비슷한 개념 
head(iris,10)
str(iris) ##summary보다 먼저해보세요!!
summary(iris) ##기본적 통계값 
iris <- data.frame(iris)

##iris라는 변수에서
iris %>% filter(Sepal.Length > 6)
sepallen_over6 <- data.frame(iris %>% filter(Sepal.Length > 6))
sepallen_over6 %>% select(Sepal.Length, Species)
iris %>% filter(Sepal.Length > 6) %>% select(Sepal.Length, Species)
iris %>%  arrange(Sepal.Length) ##정렬 // 순차적으로 정렬
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
df$ 이탈여부<-as.factor(df$이탈여부)
df$총_매출액<-as.numeric(df$총_매출액)

#파일불러오기

#install.packages("readxl")
library(readxl)

head(df)
str(df)
summary(df)

#결측치
is.na(df)
complete.cases(df)  #거꾸로 만든다 (Flase를 true로)
colSums(is.na(df))

a<-c(NA,1,2,3,4)
a<-data.frame(a)
colSums(is.na(a)) #na가 true인 갯수를 알려줌

#결측치 제거
df_del_row<- na.omit(df)    #omit 제거
summary(df_del_row)
colSums(is.na(df_del_row))

#결측치 대체
df$상품_진열_위치<-ifelse(is.na(df$상품_진열_위치),
                    mean(df$상품_진열_위치,na.rm=T),
                    df$상품_진열_위치)
colSums(is.na(df))
df$상품_설명_표시<-ifelse(is.na(df$상품_설명_표시),
                    mean(df$상품_설명_표시,na.rm=T),
                    df$상품_설명_표시)
colSums(is.na(df))

#이상치
boxplot(df$총_매출액)
hist(df$총_매출액)

iqr <-IQR(df$총_매출액)

df_iqr<-df[(df$총_매출액<median(df$총_매출액)+iqr*2)
           &(df$총_매출액>median(df$총_매출액)-iqr*2),]
hist(df_iqr$총_매출액)
hist(df$총_매출액)

df$총_매출액<-log1p(df$총_매출액)
hist(df$총_매출액)

#데이터 분해, 결합
head(df)
under_500<-df[df$고객ID<=500,]
over_500 <- df[df$고객ID>500,]
df_1000 <- rbind(under_500,over_500)
str(df_1000)

#index
df[,c("고객ID","이탈여부")]
df[150:200,c("고객ID","이탈여부")]

#merge
df1<-df[,c("고객ID","이탈여부")]
df2<-df[,c("고객ID","총_매출액")]
head(df1)
head(df2)
df_merge <- merge(df1,df2,all=T)
help(merge)
head(df_merge)

#기술통계
summary(df)
colnames(df)
df1<-df[,c("성별","할인권_사용.횟수")]
str(df1)
df1$성별 <-as.factor(df1$성별)
print(df1 %>% group_by(성별) %>% summarise(mean(할인권_사용.횟수)))
print(df1 %>% group_by(성별) %>% summarise(var(할인권_사용.횟수)))
print(df1 %>% group_by(성별) %>% summarise(sd(할인권_사용.횟수)))

df$고객등급<- as.factor(df$고객등급)
print(df %>% group_by(고객등급) %>% summarise(mean(할인권_사용.횟수)))
## %>%  ->dplyr 패키지

#t-test
##정규성검정
#shapiro-wilk
shapiro.test(df$총_매출액)
hist(df$총_매출액)
ks.test(df$총_매출액,pnorm) #정규분포 아닌게 확실해짐

shapiro.test(over_500$총_매출액)
shapiro.test(under_500$총_매출액)

#상관관계 분석 cor(df$총_매출액, df$할인권_사용.횟수)
#독립성 검정
df_chisq <- table(df$클레임접수여부, df$이탈여부)
chisq.test(df_chisq)

#등분산 검정
#install.packages("lawstat")
#help("levene.test")
library(lawstat)
df$성별<-as.factor(df$성별)
df$성별<-as.integer(df$성별)
levene.test(df$총_매출액,df$성별)
bartlett.test(df$총_매출액,df$성별)
levene.test(df$방문빈도,df$클레임접수여부)
bartlett.test(df$방문빈도,df$클레임접수여부)

df_m <- df[df$성별 ==1,]  
df_w <- df[df$성별 ==2,]  
par(mfrow=c(1,2))
hist(df_m$총_매출액)
hist(df_w$총_매출액)

#t-test
t.test(df_m$총_매출액, df_w$총_매출액,var.equal =F)

#ANOVA
anova <- aov(총_매출액 ~구매유형,data=df)
summary(anova)
print(df %>% group_by(구매유형) %>% summarise(mean(총_매출액)))








