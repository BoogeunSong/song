"hello"
a <- "hello"
a

getwd()
setwd("C:/r project/�����")
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


11%/%3 #�������� ��
11%%3   #�������� ������
2**3    #����

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

#���ڿ� ��ġ��
#head+tail
paste(head, tail, sep=" ")

#stringr Ȱ��
a="hello world"
nchar(a)
str_length(a)
#str�� ���� ����
a<- " "
a<-str_c(a,"abc")
#str�� ���� ���ֱ�
str_trim(a,side="left")


#str�� ���ڿ� �ٲٱ�
a<-"statistics"
a<-str_replace(a, "tistics", "rbucks")
a

#str split
a<-"boogeun song"
str_split(a," ")
b <- "sbg4605@naver.com"
str_split(b, "@")
a
#�빮�ڷ� �ٲٱ�
a<- toupper(a)
a
#�ҹ��ڷ� �ٲٱ�
a<- tolower(a)
a

#������(logical)
a<- T
typeof(a)
is.logical(a)
#���������� �ٲٱ�
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

#�񱳿�����
a<-1
b<-5
a>b
a<b
c<-a ==b
typeof(c)

#����
a <- c()  #combine(��ģ��)->����
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
length(d) #���͵� ���̰� �ִ�

b<-b+5
b
b<-c(-1,-2,-3,-4)
b<-b*10
b<-b*1.123
sqrt(b) #��Ʈ
abs(b)  #���밪
round(b,1)  #�Ҽ��� �����ڸ� ����
mean(b) #���
median(b) #�߾Ӱ�
var(b)  #�л�
sd(b)   #ǥ������
min(b)  #�ּҰ�
max(b)  #�ִ밪

#���(matrix)
matrix(c(1:9),nrow = 3, ncol = 3,byrow = T) #T���ϸ� �����ä��� F���ϸ� ������ ä���
matrix(c(1:9),nrow = 3, ncol = 3,byrow = F)
#�迭(array)
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
                        ȭ��ǰ=c(234,561,623,46,23,126,63,122),
                        �ķ�ǰ=c(10,20,30,40,50,60,20,10),
                        �Ƿ�=c(23,161,73,123,663,112,623,122),
                        ������ǰ=c(123,251,631,1235,622,421,512,712))
sales_data
sales_data<-sales_data[2:5]
sales_data
index=c("2016,","2017","2018","2019","2020","2021","2022","2023")
sales_data <-data.frame(data=sales_data,row.names=index)
sales_data
colnames(sales_data) <-c("ȭ��ǰ","�ķ�ǰ","�Ƿ�","������ǰ")
sales_data_jobhwa <- c(215,612,616,123,621,251,636,345)
sales_data_total <- cbind(sales_data, sales_data_jobhwa)
sales_data_total
colnames(sales_data_total)<-c("ȭ��ǰ","�ķ�ǰ","�Ƿ�","������ǰ","��ȭ")
sales_data_total
sales_data_total$��ȭ <-c(153,121,231,235,122,221,112,212)
summary(sales_data_total)
var(sales_data_total$��ȭ)
sd(sales_data_total$��ȭ)
sales_data_total[3]
sales_data_total[4,3]
sales_data_total["2019","�Ƿ�"]
sales_data_total[5:8,1:3]
subset(sales_data_total,"ȭ��ǰ">100)
subset1<-subset(sales_data_total,ȭ��ǰ>500)
subset(subset1,�ķ�ǰ>20)
#help("subset")

summary(sales_data_total) #�����͹����� �Ź� ����1
str(sales_data_total)  #�����͹����� �Ź� ����2 #str: ���� ����ִ��� ���������� ������
sales_data_total["2024",] <-c(152,623,251,235,125,212)
sales_data_total

#as. -> ������ ������ ��ȯ
#is. -> ������ Ȯ��
#factor -> ������ ������ ����


#���ǹ�
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

  
  
  
  
  
  
  
  
  
