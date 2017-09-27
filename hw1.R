library(class)

s = read.csv("D:/Shared/李御璽老師課程(資料檔)/二.使用 R 語言進行資料探勘(Data)/Data/diabetes.csv")
data = data[,-1]
head(s,3)

set.seed(107) #隨機種子
select <- sample(1:nrow(data), nrow(data)*0.8)

train = data[select,-ncol(data)]
test = data[-select,-ncol(data)]
train.y = data[select,ncol(data)]
test.y = data[-select,ncol(data)]

replacev = function(x) { 
  x = sub("tested_positive","1",x)
  x = sub("tested_negative","0",x)
  x = as.integer(x)
  return (x)
}
