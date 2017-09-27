data = read.csv("D:/Shared/李御璽老師課程(資料檔)/二.使用 R 語言進行資料探勘(Data)/Data/InsuPromotion2.csv")
head(data,3)

set.seed(107) #隨機種子
select <- sample(1:nrow(data), nrow(data)*0.8)

train = data[select,-ncol(data)]
test = data[-select,-ncol(data)]
train.y = data[select,ncol(data)]
test.y = data[-select,ncol(data)]

#預測 目標:cl = train.y
y_hat <- knn(train = train, test = test, cl = train.y, k = 1)
#計算準確度
accuracy.knn <- sum(y_hat==test.y)/length(test.y)

ROC <- data.frame()

for (i in seq(from = 1, to = 8, by = 1))
{
  y_hat <- knn(train = train, test = test, cl = train.y, k = i)
  accuracy.knn <- sum(y_hat==test.y)/length(test.y)
  out <- data.frame(i,accuracy.knn)
  ROC <- rbind(ROC,out)
}

names(ROC) <- c("n","accuracy")
ROC

num_k <- ROC[(ROC$accuracy == max(ROC$accuracy)),1]
best_k <- max(num_k)
best_k

#把準確度，k參數又最小的結果再次跑出來
y_hat <- knn(train = train, test = test, cl =train.y, k=best_k)
accuracy.knn <- sum(y_hat == test.y) / length(test.y)

#測試資料的混亂矩陣
table(y_hat,test.y)

#k參數和預測值準確度
out <- data.frame(best_k,accuracy.knn)
out
