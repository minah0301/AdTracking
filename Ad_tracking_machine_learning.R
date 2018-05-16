###패키지 불러오기
search()
#install.packages("doBy")
library(caret) #훈련용 평가용 데이터 나눌때 사용
library(data.table)

###데이터를 불러오기
d<-fread("train_sample.csv",na.strings = c("",NA),stringsAsFactors = FALSE)

###데이터 탐색
dim(d)
names(d)
str(d)
summary(d)



###click_time 
d_c<-copy(d)[,`:=`(hour=hour(d$click_time),
                   mday=mday(d$click_time),
                   year=year(d$click_time),
                   wday=wday(d$click_time),
                   mon=month(d$click_time),
                   is_attributed=factor(d$is_attributed),
                   click_time=as.POSIXct(d$click_time),
                   attributed_time=as.POSIXct(d$attributed_time)
                   )]

str(d_c)
plot(table(d_c$year)) #년도
plot(table(d_c$wday)) #요일이 0-6까지, 일요일이 0
plot(table(d_c$mday)) #한달중 며칠인지 1-31까지
plot(table(d_c$hour)) #시간이 0-23시
plot(table(d_c$mon)) #월

###is_attributed
d_a<-d_c[is_attributed==1][,ahour := hour][,aday := mday][,awday := wday]
str(d_a)

plot(table(d_a$ahour))
plot(table(d_a$aday))
plot(table(d_a$awday))

###compare click_time & is_attributed 
par(mfrow=c(3,2))
plot(table(d_c$hour))
plot(table(d_a$ahour))
plot(table(d_c$wday))
plot(table(d_a$awday))
plot(table(d_c$mday))
plot(table(d_a$aday))


d_c1<-d_a[,-6:-7] #click_time,attributed_time 제거
str(d_c1)
summary(d_c1)


###훈련용과 평가용 데이터 나누기
d_train<-d_c1[1:70000,] #방법0
d_test<-d_c1[70001:100000,]
dim(d_train);dim(d_test)
table(d$is_attributed)
table(d_train$is_attributed)
table(d_test$is_attributed)


d1<-sample(1:nrow(d_c1),size = nrow(d_c1)*0.7,replace = F) #방법1
d_train1<-d[d1,]
d_test1<-d[-d1,]
dim(d_train1);dim(d_test1)
table(d$is_attributed)
table(d_train1$is_attributed)
table(d_test1$is_attributed)



d_c2<-copy(d)[,`:=`(hour=hour(d$click_time),  #방법2 "createDataPartition"은 factor 안되고 vector됨
                   mday=mday(d$click_time),
                   year=year(d$click_time),
                   wday=wday(d$click_time),
                   mon=month(d$click_time),
                   click_time=as.POSIXct(d$click_time),
                   attributed_time=as.POSIXct(d$attributed_time)
)]
str(d_c2)

d2<-createDataPartition(d_c2$is_attributed, p=.7,list=F)
d_train2<-d_c2[d2,]
d_test2<-d_c2[-d2,]
table(d_train2$is_attributed)
table(d_test2$is_attributed)

?glm
glmModel<-glm(formula=is_attributed~app+device,data=d_c1,family=binomial)
summary(glmModel)


