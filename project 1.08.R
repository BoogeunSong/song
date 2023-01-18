"hello"
a <- "hello"
a

getwd()
setwd("C:/r project/유비온")
getwd()

a <- 1
b <- 2
a+b
a*b

typeof(a)
mode(b)
a<-as.integer(a)
typeof(a)
is.integer(a)


11%/%3 #나눗셈의 몫
11%%3   #나눗셈의 나머지
2**3    #제곱

print(a)

#install.packages("stringr")
library(stringr)
#.libPaths("C:/Program Files/R/R-3.6.3/library")


head <- "hello"
tail <-"world"

print(head)
print(tail)
help("stringr")
#install.packages("caret",dependencies = T)
??stringr
help("caret")
#install.packages("recipes", type = "binary")
library(recipes)
#install.packages("caret")
??caret
library(caret)

#문자열 합치기
#head+tail
paste(head, tail, sep=" ")

#stringr 활용
a="hello world"
nchar(a)
str_length(a)
#str로 문자 조인
a<- " "
a<-str_c(a,"abc")
#str로 공백 없애기
str_trim(a,side="left")


#str로 문자열 바꾸기
a<-"statistics"
a<-str_replace(a, "tistics", "rbucks")
a

#str split
a<-"boogeun song"
str_split(a," ")
b <- "sbg4605@naver.com"
str_split(b, "@")
a
#대문자로 바꾸기
a<- toupper(a)
a
#소문자로 바꾸기
a<- tolower(a)
a

#논리형(logical)
a<- T
typeof(a)
is.logical(a)
#논리형으로 바꾸기
a<-12
typeof(a)
is.logical(a)
a<-as.logical(a)
typeof(a)
is.logical(a)

a<-c(1,2,3,4,5)
typeof(a)
a<-as.logical(a)
typeof(a)

#비교연산자
a<-1
b<-5
a>b
a<b
c<-a ==b
typeof(c)

#벡터
a <- c()  #combine(합친다)->벡터
f<-c(2,3,4,5)
f<-as.data.frame(f)
f
b<-c(1,2,3,4)
b
c<-c("a","b","c","d")
c
d<-c(a,b,c)
d
rep(b,2)
d[1]
d[5:10]
length(d) #벡터도 길이가 있다

b<-b+5
b
b<-c(-1,-2,-3,-4)
b<-b*10
b<-b*1.123
sqrt(b) #루트
abs(b)  #절대값
round(b,1)  #소수점 이하자리 조정
mean(b) #평균
median(b) #중앙값
var(b)  #분산
sd(b)   #표준편차
min(b)  #최소값
max(b)  #최대값

#행렬(matrix)
matrix(c(1:9),nrow = 3, ncol = 3,byrow = T) #T로하면 행부터채운다 F로하면 열부터 채운다
matrix(c(1:9),nrow = 3, ncol = 3,byrow = F)
#배열(array)
array(c(1:8),dim=c(2,2,2))

#list
list(name="boogeun",weight=70)

#dataframe
df <- data.frame(a=c(1,2,3,4),
                 b=c("a","b","c","d"),
                 c=c(10,20,30,40))
df
summary(df)

sales_data <- data.frame(ID=c(1,2,3,4,5,6,7,8),
                        화장품=c(234,561,623,46,23,126,63,122),
                        식료품=c(10,20,30,40,50,60,20,10),
                        의류=c(23,161,73,123,663,112,623,122),
                        전자제품=c(123,251,631,1235,622,421,512,712))
sales_data
sales_data<-sales_data[2:5]
sales_data
index=c("2016,","2017","2018","2019","2020","2021","2022","2023")
sales_data <-data.frame(data=sales_data,row.names=index)
sales_data
colnames(sales_data) <-c("화장품","식료품","의류","전자제품")
sales_data_jobhwa <- c(215,612,616,123,621,251,636,345)
sales_data_total <- cbind(sales_data, sales_data_jobhwa)
sales_data_total
colnames(sales_data_total)<-c("화장품","식료품","의류","전자제품","잡화")
sales_data_total
sales_data_total$문화 <-c(153,121,231,235,122,221,112,212)
summary(sales_data_total)
var(sales_data_total$문화)
sd(sales_data_total$문화)
sales_data_total[3]
sales_data_total[4,3]
sales_data_total["2019","의류"]
sales_data_total[5:8,1:3]
subset(sales_data_total,"화장품">100)
subset1<-subset(sales_data_total,화장품>500)
subset(subset1,식료품>20)
#help("subset")

summary(sales_data_total) #데이터받으면 매번 진행1
str(sales_data_total)  #데이터받으면 매번 진행2 #str: 뭐가 들어있는지 구조적으로 보여줌
sales_data_total["2024",] <-c(152,623,251,235,125,212)
sales_data_total

#as. -> 강제로 변수형 변환
#is. -> 변수형 확인
#factor -> 집단형 변수에 적용


#조건문
#if, else
sales <- 600
if(sales >= 700){
  print("S")
}else{
  if(sales >= 600){
    print("A")
  }else{
    if(sales >= 500){
      print("B")
    }else{
      print("C")
    }
}
}

#ifelse
sales=400
ifelse(sales>=700,"A",ifelse(sales>=600,"B",
                              ifelse(sales>=500,"C","D")))  
  
#for
for(i in c(1:10)){
  print(i)
}

  
  
  
  
  
  
  
  
  

