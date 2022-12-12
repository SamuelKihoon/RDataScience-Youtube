

#엑셀 읽기 (소륜)
install.packages('xlsx')


library(xlsx)
library(ggplot2)


par(mfrow=c(1,1), family = "AppleSDGothicNeo-Regular")


youtuber <- read.xlsx(file = file.path('/Users/kimsoryun/Documents/rtube/RDataScience-Youtube/youtubers1.xlsx'),
                      header=T, sheetName='보물섬', as.data.frame=TRUE,
                      colIndex=c(2:7))

youtuber

total <- read.xlsx(file = file.path('/Users/kimsoryun/Documents/rtube/RDataScience-Youtube/youtubers2.xlsx'),
                   header=T, sheetName='모듬', as.data.frame=TRUE,
                   colIndex=c(3:7))

total


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

# 평균 이상 (조회수) 데이터만 추출
con.view.box_uppermean <- function() {
  upper.mean <- c()  #조회수가 평균 이상인 컨텐츠 목록
  for(i in 1:length(conlist)){
    if(mean(youtuber[which(youtuber$contents==conlist[i])
                     ,"views.1000"]) >= mean(youtuber$views.1000)) {
      upper.mean <- append(upper.mean, conlist[i])
    } #조건에 만족하는 컨텐츠를 목록에 추가
  }
  upperview <- subset(youtuber, contents %in% upper.mean) 
  #upper.mean에 있는 컨텐츠만 있는 subset 저장
  
  boxplot(views.1000~contents,  
          data=upperview,            
          main='컨텐츠별 평균 이상 조회수')
}

# 조회수-좋아요 관계
view.likes.point <- function() {
  yout.data <- youtuber[, c(2,3)] 
  total.data <- total[, c(1,2)]
  ggplot(total.data, aes(x=views.1000, y=likes.100))+
    geom_point(color='red')+
    geom_smooth(method='lm', formula = 'y~x', color='red')+
    geom_point(data=yout.data, aes(x=views.1000, y=likes.100), color='blue')+
    geom_smooth(data=yout.data, aes(x=views.1000, y=likes.100), color='blue',
                method='loess', formula = 'y~x')+
    scale_x_continuous(limits = c(0, max(yout.data$views.1000)))+
    scale_y_continuous(limits = c(0, max(yout.data$likes.100)))
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

#컨텐츠 별 예상 수익 총합
mean.by.contents('likely_return.1000')*table(youtuber$contents)

con.return.pie <- function() {
  tot <- mean.by.contents('likely_return.1000')*table(youtuber$contents)
  tot <- (tot/sum(tot))*100
  pie <- data.frame(conlist, tot)[2:3]
  colnames(pie)<- c('contents','tlr')
  ggplot(pie, aes(x='', y=tlr ,fill = factor(conlist))) +
    geom_bar(stat='identity')+
    theme_void()+
    coord_polar(theta = "y", start=0)+
    geom_text(aes(label=paste0(round(tlr,1),'%')),
              position=position_stack(vjust=0.5))
}

# 컨텐츠-좋아요수 관계 (박스플롯)
con.like.box <- function() {
  clb <- mean.by.contents('likes.100')
  boxplot(likes.100~contents,  
          data=youtuber,            
          main='컨텐츠별 좋아요수')
  max.l.con <-youtuber[which(youtuber$likes.100==max(youtuber$likes.100)),
                     'contents'] #조회수가 가장 높은 컨텐츠 저장
  
  max.l.mean <- names(clb)[clb== max(clb)] #평균 좋아요수가 가장 높은 컨텐츠 저장
  cat('좋아요수가 가장 높은 영상의 컨텐츠는',max.l.con,'입니다.')
  cat('평균 좋아요수가 가장 높은 영상의 컨텐츠는',max.l.mean,'입니다.')
}

# 평균 이상 (좋아요수) 데이터만 추출
con.like.box_uppermean <- function() {
  upper.l.mean <- c()  #좋아요수가 평균 이상인 컨텐츠 목록
  for(i in 1:length(conlist)){
    if(mean(youtuber[which(youtuber$contents==conlist[i])
                     ,"likes.100"]) >= mean(youtuber$likes.100)) {
      upper.l.mean <- append(upper.l.mean, conlist[i])
    } #조건에 만족하는 컨텐츠를 목록에 추가
  }
  upperlike <- subset(youtuber, contents %in% upper.l.mean) 
  #upper.l.mean에 있는 컨텐츠만 있는 subset 저장
  
  boxplot(likes.100~contents,  
          data=upperlike,            
          main='컨텐츠별 평균 이상 좋아요수')
}

install.packages('fmsb')
library(fmsb)

x <- mean.by.contents('views.1000')*table(youtuber$contents)
df<-as.data.frame(x)
df <- t(df)
df
con.view.radar <- function()
radarchart(df, # 데이터프레임
           pcol='dark green', # 다각형선의색
           pfcol=rgb(0.2,0.5,0.5,0.5), # 다각형내부색
           plwd=3, # 다각형선의두께
           cglcol='grey', # 거미줄의색
           cglty=1, # 거미줄의타입
           cglwd=0.8, # 거미줄의두께
           axistype=1, # 축의레이블타입
           seg=4, # 축의눈금분할
           axislabcol='grey', # 축의레이블색
           caxislabels=seq(0,100,25) # 축의레이블값
)


#추천 컨텐츠 정하기
con.rec <- function() {
  over_10p.list <- conlist[table(youtuber$contents)>=5]
  over_10p.sub <- subset(youtuber, contents %in% over_10p.list)
  conlist <- sort(unique(over_10p.sub$contents)) 
  mean.by.contents <- function(subject) {
    result <- c()
    for(i in 1:length(conlist)) {
      result <- append(result, mean(over_10p.sub[which(over_10p.sub$contents==conlist[i]),
                                                 subject]))
    }
    names(result) <- conlist
    return(result)
  }
  subtlr <- mean.by.contents('likely_return.1000')*table(over_10p.sub$contents) #총 예상수익
  subv <- mean.by.contents('views.1000') #조회수
  sublr <- mean.by.contents('likes.rate')  #좋아요 비율
  
  score <- c()
  tmp.order <- order(subtlr)
  for(j in 1:length(tmp.order)) {
    score[tmp.order[j]] <- j
  }
  tmp.order <- order(subv)
  for(j in 1:length(tmp.order)) {
    score[tmp.order[j]] <- score[tmp.order[j]]+ j*5
  }
  tmp.order <- order(sublr)
  for(j in 1:length(tmp.order)) {
    score[tmp.order[j]] <- score[tmp.order[j]]+ j*5
  }
  link <- c()
  for(i in 1:length(conlist)) {
    p <- subset(over_10p.sub, contents==conlist[i])
    link <- append(link, p[which(p$views.1000==max(p$views.1000)),'link'])
  }
  ds <- as.data.frame(cbind(subtlr, subv, sublr, score,link))
  colnames(ds) <- c('총 예상수익','평균 조회수','좋아요 비율','추천도','추천 영상 링크')
  ds[order(ds$추천도, decreasing = TRUE),]
} 

con.view.box()
con.view.box_uppermean()
view.likes.point()
view.likes.bar()
top10()
youtuber
con.return.pie()
con.like.box()
con.like.box_uppermean()
con.rec()
