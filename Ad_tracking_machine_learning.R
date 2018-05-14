###패키지 불러오기
search()
install.packages("doBy")
library(caret) #훈련용 평가용 데이터 나눌때 사용

###데이터를 불러오기
d<-fread("train_sample.csv",na.strings = c("",NA),stringsAsFactors = FALSE)

###데이터 탐색
str(d)
names(d)
dim(d)

###변수 탐색 및 변경
d_c<-copy(d)[,`:=`(hour=hour(d$click_time),
                   mday=mday(d$click_time),
                   year=year(d$click_time),
                   wday=wday(d$click_time),
                   mon=month(d$click_time),
                   is_attributed=factor(d$is_attributed),
                   click_time=as.POSIXct(d$click_time)
                   )]

str(d_c)
plot(table(d_c$year)) #년도
plot(table(d_c$wday)) #요일이 0-6까지, 일요일이 0
plot(table(d_c$mday)) #한달중 며칠인지 1-31까지
plot(table(d_c$hour)) #시간이 0-23시
plot(table(d_c$mon)) #월

d_c1<-d_c[,-6:-7] #click_time,attributed_time 제거
str(d_c1)
summary(d_c1)


###훈련용과 평가용 데이터 나누기
d_train<-d[1:70000,] #방법0
d_test<-d[70001:100000,]
dim(d_train);dim(d_test)
table(d$is_attributed)
table(d_train$is_attributed)
table(d_test$is_attributed)


d1<-sample(1:nrow(d),size = nrow(d)*0.7,replace = F) #방법1
d_train1<-d[d1,]
d_test1<-d[-d1,]
dim(d_train1);dim(d_test1)
table(d$is_attributed)
table(d_train1$is_attributed)
table(d_test1$is_attributed)


d2<-createDataPartition(d$is_attributed, p=.7,list=F) #방법2
d_train2<-d[d2,]
d_test2<-d[-d2,]
table(d_train2$is_attributed)
table(d_test2$is_attributed)

install.packages("lightgbm")
library(lightgbm)

