data2021 <- read.csv("도로교통공단_사망교통사고정보(2021).csv" , stringsAsFactors = F , header = T, 
                fileEncoding ='cp949')
data2020 <- read.csv("도로교통공단_사망교통사고정보(2020).csv" , stringsAsFactors = F , header = T, 
                     fileEncoding ='cp949')
data2019 <- read.csv("도로교통공단_사망교통사고정보(2019).csv" , stringsAsFactors = F , header = T, 
                     fileEncoding ='cp949')
data2018 <- read.csv("도로교통공단_사망교통사고정보(2018).csv" , stringsAsFactors = F , header = T, 
                     fileEncoding ='cp949')
colnames(data2018)[6] <- "부상자수"
ods<-rbind(data2021,data2020,data2019,data2018) #df합치기
colnames(ods)[2] <- "ymd"
ods$ymd <- as.Date( ods$ymd )
summary( ods)
options( digits=5 )
Totals <- nrow( ods )
library(dplyr)
ymd2018 <- ods%>% filter(발생년==2018)
ymd2019 <- ods%>% filter(발생년==2019)
ymd2020 <- ods%>% filter(발생년==2020)
ymd2021 <- ods%>% filter(발생년==2021)
TotalYmd1 <- as.data.frame( ymd2018)
TotalYmd2 <- as.data.frame( ymd2019)
TotalYmd3 <- as.data.frame(ymd2020)
TotalYmd4 <- as.data.frame( ymd2021)
TotalYmd <- as.data.frame(table( ods$ymd ))
colnames(TotalYmd) <- c("ymd","x")
colnames(TotalYmd1) <- c("ymd","x")
colnames(TotalYmd2) <- c("ymd","x")
colnames(TotalYmd3) <- c("ymd","x")
colnames(TotalYmd4) <- c("ymd","x")
Countreason <- table( ods$가해자법규위반)
TotalReason <- as.data.frame(Countreason)
TotalReason$per <- 100*TotalReason$Freq/Totals
Countdeath <- table( ods $사망자수)
Totaldeath<- as.data.frame(Countdeath)
par(mars<-c(2,2,2,2))
library(reshape)
# Column Subset
# 필요한 컬럼만 선택하고, 컬럼이름 부여하기
mycols <- c(2,14,15)

dataset <- ods[ , mycols ]            
colnames(dataset) <- c("ymd","사고유형","가해자법규위반")

TopReason <- TotalReason[ order(-TotalReason$Freq),c("Var1","Freq") ]
TopReason <- TopReason[1:10,] 
colnames( TopReason ) <- c("가해자법규위반","사고수")

data <- merge(x=dataset,y=TopReason,by='가해자법규위반')
data <- data[,c("ymd","가해자법규위반","사고유형")]
YmdReason <- cast(data, ymd~가해자법규위반 )

# Top1 12번 사망원인으로 연도별 시계열 분석 및 시각화
timeseries <- ts(YmdReason$"12", c(2018,01,01), frequency = 364)
tsdecomp  <- decompose(timeseries)
plot(tsdecomp)
ts_data <- data.frame(seq(1:1460)
                       ,TotalYmd$ymd
                       ,tsdecomp$x  
                       ,tsdecomp$trend  
                       ,tsdecomp$seasonal  
                       ,tsdecomp$random )
colnames(ts_data) <- c("순서","년월일","X","T","S","R")
ts_data$i <- (ts_data$순서-1)%/%364
ts_data$j <- (ts_data$순서-1)%%364

ts1 <- ts_data[ ts_data$i==0, ]  #2018
plot( ts1$j, ts1$X, col='black', type='l', ylim=c(0,30)) #ylim은 크기조정
ts2 <- ts_data[ ts_data$i==1, ] #2019
lines(ts2$j, ts2$X, col='red')
ts3 <- ts_data[ ts_data$i==2, ] #2020
lines(ts3$j, ts3$X, col='blue')
ts4 <- ts_data[ ts_data$i==3, ] #2021
lines(ts4$j, ts4$X, col='green')
title("교통사고 사건수- observed")

ts1 <- ts_data[ ts_data$i==0, ]  #2018
plot( ts1$j, ts1$T, col='black', type='l', ylim=c(0,30)) #ylim은 크기조정
ts2 <- ts_data[ ts_data$i==1, ] #2019
lines(ts2$j, ts2$T, col='red')
ts3 <- ts_data[ ts_data$i==2, ] #2020
lines(ts3$j, ts3$T, col='blue')
ts4 <- ts_data[ ts_data$i==3, ] 
lines(ts4$j, ts4$T, col='green')
title("교통사고 사건수- trend")

ts1 <- ts_data[ ts_data$i==0, ]  #2018
plot( ts1$j, ts1$S, col='black', type='l', ylim=c(0,30)) #ylim은 크기조정
ts2 <- ts_data[ ts_data$i==1, ] #2019
lines(ts2$j, ts2$S, col='red')
ts3 <- ts_data[ ts_data$i==2, ] 
lines(ts3$j, ts3$S, col='blue')
ts4 <- ts_data[ ts_data$i==3, ] 
lines(ts4$j, ts4$S, col='green')
title("교통사고 사건수-seasonal")  #원래 계절성 그래프 하나로 나옴

ts1 <- ts_data[ ts_data$i==0, ]  #2018
plot( ts1$j, ts1$R, col='black', type='l', ylim=c(0,10)) #ylim은 크기조정
ts2 <- ts_data[ ts_data$i==1, ] #2019
lines(ts2$j, ts2$R, col='red')
ts3 <- ts_data[ ts_data$i==2, ] 
lines(ts3$j, ts3$R, col='blue')
ts4 <- ts_data[ ts_data$i==3, ]
lines(ts4$j, ts4$R, col='green')
title("교통사고 사건수- Random")  # 원래 이동평균 6개월이라서 2018,2021 초, 말 6개월 na임 

# 시계열분해 코드 
timeseries <- ts(TotalYmd$x, c(2018,01,01), frequency = 364)
tsdecomp <- decompose(timeseries) 
plot(tsdecomp)
ts_data <- data.frame( TotalYmd$ymd
                       ,tsdecomp$x  
                       ,tsdecomp$trend  
                       ,tsdecomp$seasonal  
                       ,tsdecomp$random )
#------
barplot( Countreason[1:18], main="가해자 법규위반", xlab="가해자 법규위반", ylab="교통사고수" ) #이유 18개 밖에없음

# 열 두개 선택( 사고유형(age)별 가해자법규위반(사망원인코드)  )
# 분석 대상 선정 -> 정면충돌 사고원인
CountAge <-  table( ods$사고유형 ) 
TotalAge <- as.data.frame(CountAge)
factor사고유형<-factor(ods$사고유형)

summary(data); str(data)

#11주차
AccidentTable <- table(data$사고유형, data$가해자법규위반)
AccidentDF <- data.frame( AccidentTable )
colnames( AccidentDF ) <- c("사고유형","가해자법규위반","사고수")
library(reshape)
ReasonAccdient <- cast( AccidentDF, 가해자법규위반~사고유형, 
                   value='사고수', fun.aggregate=sum )
# 비교 시각화
barplot( AccidentTable, main="사고유형별 사고원인"
         ,xlab="사고 원인", ylab="사고 유형", col=rainbow(16) ) #사고유형개수
legend("topright",levels(TotalAge$Var1), fill=rainbow(16),cex=0.8)  #범례

#집계표 (stack chart)
AccidentTable2 <- prop.table( AccidentTable, 2)
AccidentDF2 <- data.frame( AccidentTable2 ) 
AccidentDF2$Freq <- round(AccidentDF2$Freq,3)
colnames( AccidentDF2 ) <- c("사고유형","사고원인","사고유형별.비율")
barplot( AccidentTable2, main="사고유형별 사고원인"
         ,xlab="사고원인", ylab="사고유형", col=rainbow(16) )
legend("topright",levels(TotalAge$Var1), fill=rainbow(16),cex=0.8)  #범례

# 사고유형별 가해자법규위반를 비율로 변경 (군집)
ReasonAccdient$t1 <- ReasonAccdient[2]+ReasonAccdient[3]+ReasonAccdient[4]+ReasonAccdient[5]+ReasonAccdient[6]
ReasonAccdient$t2 <- ReasonAccdient[7]+ReasonAccdient[8]+ReasonAccdient[9]+ReasonAccdient[10]+ReasonAccdient[11]
ReasonAccdient$total <- ReasonAccdient[12]+ReasonAccdient[13]
ReasonAccdient$Accdient0 <- 100*ReasonAccdient[2]/ReasonAccdient[14]
ReasonAccdient$Accdient1 <- 100*ReasonAccdient[3]/ReasonAccdient[14]
ReasonAccdient$Accdient2 <- 100*ReasonAccdient[4]/ReasonAccdient[14]
ReasonAccdient$Accdient3 <- 100*ReasonAccdient[5]/ReasonAccdient[14]
ReasonAccdient$Accdient4 <- 100*ReasonAccdient[6]/ReasonAccdient[14]
ReasonAccdient$Accdient5 <- 100*ReasonAccdient[7]/ReasonAccdient[14]
ReasonAccdient$Accdient6 <- 100*ReasonAccdient[8]/ReasonAccdient[14]
ReasonAccdient$Accdient7 <- 100*ReasonAccdient[9]/ReasonAccdient[14]
ReasonAccdient$Accdient8 <- 100*ReasonAccdient[10]/ReasonAccdient[14]
ReasonAccdient$Accdient9 <- 100*ReasonAccdient[11]/ReasonAccdient[14]

ReasonAccdient2 <- data.frame( ReasonAccdient[1],ReasonAccdient[15:18] )
names(ReasonAccdient2)[2:5] <- colnames(ReasonAccdient)[2:5]
colnames(ReasonAccdient2)  

# 사고 원인별 유사도 계산 및 시각화
rownames(ReasonAccdient2) <- ReasonAccdient2[,1]
ReasonAccdient2 <- ReasonAccdient2[-1]
ReasonDist <- dist(ReasonAccdient2, method="manhattan")
two_coord <- cmdscale(ReasonDist)
plot(two_coord, type="n", xlab="x", ylab="y")
text(two_coord, rownames(ReasonAccdient2) )

plot(two_coord, type="n", xlab="x", ylab="y", xlim=c(470,520),ylim=c(10,32))
text(two_coord, rownames(ReasonAccdient2) )


# 계층적 군집
library( cluster )
hcl <- hclust( dist(ReasonAccdient2, method="manhattan"), method="single")
plot(hcl, hang=-1, xlab="사고원인", ylab="거리")

# 분할적 군집
library( graphics )
kms <- kmeans( ReasonAccdient2, 4 )
kms 

# bubble chart
library(MASS)
library("ggplot2")
str(ods)
ods <- ods %>% mutate(총피해자수= 사망자수+부상자수+중상자수+경상자수)
levels(ods$가해자법규위반)
table(ods$가해자법규위반)
library(sqldf)
data_sample<-subset(ods, select=c("발생위치Y_UTMK","발생위치X_UTMK","총피해자수"))
# Bubble chart 그리기 
ggplot(data_sample, aes(x=발생위치X_UTMK, y=발생위치Y_UTMK)) +
 geom_point(aes(size=총피해자수), shape=21, colour="grey90", fill="orange", alpha=0.5)+
 scale_size_area(max_size = 15) + # 범례 없애려면 guide=FALSE
 ggtitle("Bubble chart with Number of traffic accident victims by location") 

# bubble chart
library(MASS)
library("ggplot2")
str(ods)
levels(ods$가해자법규위반)
table(ods$가해자법규위반)
library(sqldf)
data_sample<-subset(ods, select=c("가해자법규위반","사고유형","총피해자수"))
# Bubble chart 그리기 
ggplot(data_sample, aes(x=가해자법규위반, y=사고유형)) +
  geom_point(aes(size=총피해자수), shape=21, colour="grey90", fill="orange", alpha=0.5)+
  scale_size_area(max_size = 15) + # 범례 없애려면 guide=FALSE
  ggtitle("Bubble chart with Number of traffic accident victims")

#트리맵 메모리 로드
library(treemap)
ods <- ods %>% mutate(총피해자수= 사망자수+부상자수+중상자수+경상자수)

#트리맵 그리기 
treemap(ods ,vSize = "총피해자수", index = c("가해자법규위반","발생지시도"), title = "지역별 가해자법규위반")
