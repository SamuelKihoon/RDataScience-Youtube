
#git1234
#12345


#RMarkdown
#Kntir
install.packages('rmarkdown')
install.packages('knitr')



#엑셀 읽기
install.packages('xlsx')
library(xlsx)
library(ggplot2)
youtuber <- read.xlsx(file = file.path('C:/Rworks/youtubers2.xlsx'),
                      header=T, sheetName='하승진', as.data.frame=TRUE,
                      colIndex=c(2:7))

total <- read.xlsx(file = file.path('C:/Rworks/youtubers2.xlsx'),
                   header=T, sheetName='모듬', as.data.frame=TRUE,
                   colIndex=c(3:7))

youtuber$likes.rate <- round((youtuber$likes/youtuber$views.1000)*10,digit=2) #좋아요 비율 만들기
conlist <- sort(unique(youtuber$contents))  #컨텐츠 종류 저장

mean.by.contents <- function(subject) {
  result <- c()
  for(i in 1:length(conlist)) {
    result <- append(result, mean(youtuber[which(youtuber$contents==conlist[i]),
                                           subject])) 
  }
  names(result) <- conlist
  return(result)
}   #컨텐츠별로 subject의 평균 계산 함수


mycol <- c()
mycol[4:length(conlist)] <- '#B8E089'

# 컨텐츠-조회수 관계(박스플롯)
con.view.box <- function() {
  max.con <-youtuber[which(youtuber$views.1000==max(youtuber$views.1000)),
                     'contents'] #조회수가 가장 높은 컨텐츠 저장
  vbc <- sort(mean.by.contents('views.1000'), decreasing=TRUE)
  youtuber$contents<- factor(youtuber$contents, levels=names(vbc))
  mycol[1:3] <- '#89BAE0'
  boxplot(views.1000~contents,  
          data=youtuber,            
          main='컨텐츠별 조회수',
          col = mycol)
  
  max.mean <- names(vbc)[vbc== max(vbc)] #평균 조회수가 가장 높은 컨텐츠 저장
  cat('조회수가 가장 높은 영상의 컨텐츠는',max.con,'입니다.','\n')
  cat('평균 조회수가 가장 높은 영상의 컨텐츠는',max.mean,'입니다.')
}




# 평균 이상 데이터만 추출
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
          main='컨텐츠별 조회수',
          col='#89BAE0')
}

# 조회수-좋아요 관계
view.likes.point <- function() {
  yout.data <- youtuber[, c(2,3,6)] #조회수와 좋아요 데이터만 추출
  total.data <- total[, c(1,2,5)]
  ggplot(total.data, aes(x=views.1000, y=likes.100))+
    geom_point(color='red')+
    geom_smooth(method='lm', formula = 'y~x', color='red')+ #추세선 추가
    geom_point(data=yout.data, aes(x=views.1000, y=likes.100), color='blue')+
    geom_smooth(data=yout.data, aes(x=views.1000, y=likes.100), color='blue',
                method='loess', formula = 'y~x')+
    scale_x_continuous(limits = c(0, max(yout.data$views.1000)))+ #x,y축의 limit을 유튜버 데이터의 최댓값으로 고정
    scale_y_continuous(limits = c(0, max(yout.data$likes.100)))
}


#컨텐츠별 조회수/좋아요 비율
view.likes.bar <- function() {
  color = c()
  color[1:length(conlist)] = '#76d6b0'
  color[1]='#579e82'
  ds <- mean.by.contents('likes.rate')
  sort(ds, decreasing=TRUE)
  barplot(sort(ds, decreasing=TRUE), main='좋아요/조회수',
          xlab='컨텐츠',
          ylab='비율(%)',
          col = color)
}


#상위 10개 영상 출력
top10 <- function() {
  view.t10 <- youtuber[order(-youtuber$views.1000),
                       c('contents','views.1000','likes.100','likes.rate')][1:10,]
  #조회수로 정렬후 상위10개 영상의 데이터 저장
  print(view.t10)
  con.num <- table(view.t10$contents)
  con.mode <- names(con.num)[con.num == max(con.num)] #탑10의 컨텐츠 최빈값
  cat('조회수 탑10에 가장 많은 컨텐츠는',con.mode,'입니다.')
}




#예상 수익 비율
con.return.pie <- function() {
  tot <- mean.by.contents('likely_return.1000')*table(youtuber$contents) #총 예상수익
  tot <- (tot/sum(tot))*100  # 비율을 %로 저장
  pie <- data.frame(conlist, tot)[2:3] #데이터 프레임 형성 후 컨텐츠와 예상수익만 저장
  colnames(pie)<- c('contents','tlr')
  ggplot(pie, aes(x='', y=tlr ,fill = factor(conlist))) +
    geom_bar(stat='identity')+ #막대 그래프 형성
    theme_void()+
    coord_polar(theta = "y", start=0)+ #막대그래프를 원그래프로 전환
    geom_text(aes(label=paste0(round(tlr,1),'%')), #비중(%)표시
              position=position_stack(vjust=0.5))
}

# 컨텐츠-좋아요수 관계 (박스플롯)
con.like.box <- function() {
  max.l.con <-youtuber[which(youtuber$likes.100==max(youtuber$likes.100)),
                     'contents']  #좋아요가 가장 높은 컨텐츠 저장
  clb <- mean.by.contents('likes.100')
  clb <- sort(mean.by.contents('likes.100'), decreasing=TRUE)
  mycol[1:3] <- '#E09989'
  youtuber$contents<- factor(youtuber$contents, levels=names(clb))
  boxplot(likes.100~contents,  
          data=youtuber,            
          main='컨텐츠별 좋아요수',
          col=mycol)
  
  max.l.mean <- names(clb)[clb== max(clb)] #평균 좋아요수가 가장 높은 컨텐츠 저장
  cat('좋아요수가 가장 높은 영상의 컨텐츠는',max.l.con,'입니다.','\n')
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
          main='컨텐츠별 평균 이상 좋아요수',
          col='#E09989')
}



#추천 컨텐츠 정하기
con.rec <- function() {
  over_10p.list <- conlist[table(youtuber$contents)>=5] #5번 이상 등장한 컨텐츠 저장
  over_10p.sub <- subset(youtuber, contents %in% over_10p.list) 
  conlist <- sort(unique(over_10p.sub$contents)) #섭셋의 컨텐츠 리스트로 재설정
  mean.by.contents <- function(subject) {
    result <- c()
    for(i in 1:length(conlist)) {
      result <- append(result, mean(over_10p.sub[which(over_10p.sub$contents==conlist[i]),
                                             subject]))
    } 
    names(result) <- conlist
    return(result)
  }#섭셋에서 작동하는 함수로 재설정
  subtlr <- floor(mean.by.contents('likely_return.1000')*table(over_10p.sub$contents)) #총 예상수익
  subv <- floor(mean.by.contents('views.1000')) #조회수
  sublr <- round(mean.by.contents('likes.rate'),digit=2)  #좋아요 비율
  
  score <- c()  #점수를 저장할 벡터
  tmp.order <- order(subtlr)
  for(j in 1:length(tmp.order)) {
    score[tmp.order[j]] <- j*5
  } #총 예상수익의 등수에 따라 점수 배정
  tmp.order <- order(subv)
  for(j in 1:length(tmp.order)) {
    score[tmp.order[j]] <- score[tmp.order[j]]+ j*5
  } #평균 조회수 등수에 따라 점수 배정
  tmp.order <- order(sublr)
  for(j in 1:length(tmp.order)) {
    score[tmp.order[j]] <- score[tmp.order[j]]+ j
  } #좋아요 비율 등수에 따라 점수 배정
  link <- c()  #컨텐츠의 추천 영상을 저장할 벡터
  for(i in 1:length(conlist)) {
    p <- subset(over_10p.sub, contents==conlist[i])
    link <- append(link, p[which(p$views.1000==max(p$views.1000)),'link'])
  } #각 컨텐츠별 가장 조회수가 높은 영상의 링크를 저장
  ds <- as.data.frame(cbind(subtlr, subv, sublr, score,link))
  colnames(ds) <- c('총 예상수익','평균 조회수','좋아요 비율','추천도','추천 영상 링크')
  ds[order(ds$추천도, decreasing = TRUE),] #추천도에 따라 정렬후 출력
}  




con.view.box()
con.view.box_uppermean()
con.like.box()
con.like.box_uppermean()
view.likes.bar()
view.likes.point()
top10()
con.return.pie()
con.rec()




