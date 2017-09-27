data = read.csv("D:/Shared/李御璽老師課程(資料檔)/二.使用 R 語言進行資料探勘(Data)/Data/broadband.csv")
data = data[,-1]
head(data,3)

#把CHANNEL切出來變成向量
data.c = data[,c("CHANNEL")]
data.c = as.factor(data.c)


data.n = data[,-c(4,ncol(data))]

data.y = data[,ncol(data)]
data.y = as.factor(data.y)

normalize <- function(x) { 
  return((x - min(x)) / (max(x) - min(x)))
}

data.n <- as.data.frame(lapply(data.n, normalize))
library(nnet)
dummy.c = class.ind(data[,4])
dummy.c = model.matrix(~data.c,data.c)
dummy.c = dummy.c[,-c(1)]

dummy.c = class.ind(data.c)

dummy.c = model.matrix(~data.c,data.c)
dummy.c = dummy.c[,-c(1)]

data = cbind(dummy.c, data.n, BROADBAND = data.y)
names(data)[1:4] = c("Channel-1","Channel-2", "Channel-3", "Channel-4")

set.seed(107)
select <- sample(1:nrow(data), nrow(data)*0.8)
train.x = data[select,-ncol(data)]
test.x  = data[-select,-ncol(data)]
train.y = data[select,ncol(data)]
test.y  = data[-select,ncol(data)]

y_hat <- knn(train = train.x, test = test.x, cl = train.y, k = 1)
accuracy.knn<-sum(y_hat==test.y)/length(test.y)

table(y_hat,test.y)

ROC <- data.frame()

for (i in seq(from = 1, to = 8, by = 1))
{
  y_hat <- knn(train = train.x, test = test.x, cl = train.y, k = i)
  accuracy.knn <- sum(y_hat==test.y)/length(test.y)
  out <- data.frame(i,accuracy.knn)
  ROC <- rbind(ROC,out)
}

names(ROC) <- c("k","accuracy")
ROC
