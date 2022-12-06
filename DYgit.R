
#git

#엑셀 읽기
install.packages('xlsx')
library(xlsx)
youtuber <- read.xlsx(file = file.path('C:/Rworks/youtubers.xlsx'),
                      header=T, sheetName='매직박', as.data.frame=TRUE,
                      colIndex=c(2:6))

conlist <- unique(youtuber$contents)

mean.by.contents <- function(subject) {
  result <- c()
  for(i in 1:length(conlist)) {
    result <- append(result, mean(youtuber[which(youtuber$contents==conlist[i]),
                                           subject]))
  }
  return(result)
}


# 컨텐츠-조회수 관계(박스플롯)
unique(youtuber$contents)
boxplot(views.1000~contents,  #데이터 ~ 그룹 정보
        data=youtuber,            #데이터가 지정된 자료구조(출처)
        main='컨텐츠별 조회수')


# 평균 이상 데이터만 추출
upper.mean <- c()
for(i in 1:length(conlist)){
  if(mean(youtuber[which(youtuber$contents==conlist[i])
                   ,"views.1000"]) >= mean(youtuber$views.1000)) {
    upper.mean <- append(upper.mean, conlist[i])
  }
}
upperview <- subset(youtuber, contents %in% upper.mean)
boxplot(views.1000~contents,  #데이터 ~ 그룹 정보
        data=upperview,            #데이터가 지정된 자료구조(출처)
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


