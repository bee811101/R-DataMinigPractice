#KNN建模,需要極值正規化 介於0~1
library(class)

s = read.csv("D:/Shared/李御璽老師課程(資料檔)/二.使用 R 語言進行資料探勘(Data)/Data/default.csv")
head(s,3)

#目標屬性y
y <- s[,c("Default")]
#輸入屬性x
x <- s[,c("Income","Age","Edu.1","Edu.2","Edu.3")]

#極值正規化
normalize <- function(x) { 
  return((x - min(x)) / (max(x) - min(x)))
}

#輸入屬性套用極值正規化的函式
x <- as.data.frame(lapply(x, normalize))
              
data <- cbind(y,x)
data$y <- as.factor(data$y)

#抽樣80%
select <- sample(1:nrow(data),length(data$y)*0.8)

#訓練資料(第一個欄位不要)
train = data[select,-1]
#測試資料
test = data[-select,-1]
#答案
train.y = data[select,1]
test.y = data[-select,1]

y_hat <- knn(train = train, test = test, cl=train.y, k=1)
#正確率y_hat==test.y答案test.y
accuracy.knn <- sum(y_hat==test.y)/length(test.y)
accuracy.knn
agreement_KNN <- y_hat==test.y
agreement_KNN

              