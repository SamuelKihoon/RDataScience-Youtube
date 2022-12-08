
#git

#엑셀 읽기
install.packages('xlsx')
library(xlsx)
youtuber <- read.xlsx(file = file.path('C:/Rworks/youtubers1.xlsx'),
                      header=T, sheetName='옥냥이', as.data.frame=TRUE,
                      colIndex=c(2:7))
total <- read.xlsx(file = file.path('C:/Rworks/youtubers2.xlsx'),
                   header=T, sheetName='모듬', as.data.frame=TRUE,
                   colIndex=c(3:7))

youtuber$likes.rate <- youtuber$likes/youtuber$views.1000 #좋아요 비율 만들기
conlist <- unique(youtuber$contents)  #컨텐츠 종류 저장

mean.by.contents <- function(subject) {
  result <- c()
  for(i in 1:length(conlist)) {
    result <- append(result, mean(youtuber[which(youtuber$contents==conlist[i]),
                                           subject]))
  }
  names(result) <- conlist
  return(result)
}   #컨텐츠별로 subject의 평균 계산 함수

# 컨텐츠-조회수 관계(박스플롯)
con.view.box <- function() {
  vbc <- mean.by.contents('views.1000')
  boxplot(views.1000~contents,  
          data=youtuber,            
          main='컨텐츠별 조회수')
  max.con <-youtuber[which(youtuber$views.1000==max(youtuber$views.1000)),
                     'contents'] #조회수가 가장 높은 컨텐츠 저장
  
  max.mean <- names(vbc)[vbc== max(vbc)] #평균 조회수가 가장 높은 컨텐츠 저장
  cat('조회수가 가장 높은 영상의 컨텐츠는',max.con,'입니다.')
  cat('평균 조회수가 가장 높은 영상의 컨텐츠는',max.mean,'입니다.')
}

# 평균 이상 데이터만 추출
con.view.box_uppermean <- function() {
  upper.mean <- c()  #조회수가 평균 이상인 컨텐츠 목록
  for(i in 1:length(conlist)){
    if(mean(youtuber[which(youtuber$contents==conlist[i])
                     ,"views.1000"]) >= mean(youtuber$views.1000)) {
      upper.mean <- append(upper.mean, conlist[i])
    } #조건의 만족하는 컨텐츠를 목록에 추가
  }
  upperview <- subset(youtuber, contents %in% upper.mean) 
  #upper.mean에 있는 컨텐츠만 있는 subset 저장
  
  boxplot(views.1000~contents,  
          data=upperview,            
          main='컨텐츠별 조회수')
}

# 조회수-좋아요 관계
view.likes.point <- function() {
  yout.data <- youtuber[, c(2,3,6)] 
  total.data <- total[, c(1,2,5)]
  merged.data <- rbind(yout.data, total.data)
  color <- c('red','blue')
  plot(merged.data$views.1000, merged.data$likes.100,
       main='조회수-좋아요 그래프',
       xlab='조회수', ylab='좋아요',
       col=color[merged.data$ctg], pch=c(merged.data$ctg))   #점의 모양 지정
}

#컨텐츠별 조회수/좋아요 비율
view.likes.bar <- function() {
  ds <- mean.by.contents('likes.rate')
  barplot(ds, main='좋아요/조회수',
          xlab='컨텐츠',
          ylab='비율')
}

#상위 10개 영상 출력
top10 <- function() {
  view.t10 <- youtuber[order(-youtuber$views.1000),
                       c('contents','views.1000','likes.100','likes.rate')][1:10,]
  
  print(view.t10)
}

#추천 컨텐츠 정하기
over_10p.list <- conlist[table(youtuber$contents)>=5]


con.view.box()
con.view.box_uppermean()
view.likes.point()
view.likes.bar()
top10()
youtuber


