
#git

#엑셀 읽기
install.packages('xlsx')
library(xlsx)
youtuber <- read.xlsx(file = file.path('C:/Rworks/youtubers.xlsx'),
                      header=T, sheetName='보물섬', as.data.frame=TRUE,
                      colIndex=c(2:6))

conlist <- unique(youtuber$contents)  #컨텐츠 종류 저장

mean.by.contents <- function(subject) {
  result <- c()
  for(i in 1:length(conlist)) {
    result <- append(result, mean(youtuber[which(youtuber$contents==conlist[i]),
                                           subject]))
  }
  return(result)
}   #컨텐츠별로 subject의 평균 계산 함수


# 컨텐츠-조회수 관계(박스플롯)
boxplot(views.1000~contents,  
        data=youtuber,            
        main='컨텐츠별 조회수')


# 평균 이상 데이터만 추출
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

# 조회수-좋아요 관계
plot(youtuber$views.1000, youtuber$likes.100,
     main='조회수-좋아요 그래프',
     xlab='조회수', ylab='좋아요',
     col='red', pch=19)   #점의 모양 지정

#컨텐츠별 조회수/좋아요 비율
youtuber$likes.rate <- youtuber$likes/youtuber$views.1000
ds <- mean.by.contents('likes.rate'); names(ds) <- conlist
barplot(ds, main='좋아요/조회수',
        xlab='컨텐츠',
        ylab='비율')

#상위 5개 영상 출력
view.t10 <- youtuber[order(-youtuber$views.1000),
                    c('contents','views.1000','likes.100','likes.rate')][1:10,]

print(view.t10)

