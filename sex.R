s = read.csv("D:/Shared/李御璽老師課程(資料檔)/二.使用 R 語言進行資料探勘(Data)/Data/sex.csv")
head(s,3)

#最後一個欄位不要
train.x = s[,-ncol(s)]
train.y = s[,ncol(s)]

#把yes/no=>轉成字串=>轉成數字
replacev = function(x) { 
  x = sub("Yes","1",x)
  x = sub("No","0",x)
  x = as.integer(x)
  return (x)
}

train.x <- as.data.frame(lapply(train.x, replacev))
s  = cbind(train.x, sex = train.y)
