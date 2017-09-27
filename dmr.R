
library(bnlearn)

s = read.csv("D:/Shared/李御璽老師課程(資料檔)/二.使用 R 語言進行資料探勘(Data)/Data/sex.csv")
head(s,3)
#ceiling無條件進位
np = ceiling(0.2*nrow(s)) 
test.index = sample(1:nrow(s), np)
sex.test = s[test.index,]
sex.train = s[-test.index,]

bn = naive.bayes(sex.train, "sex")
plot(bn)

pred1 = predict(bn, sex.train, prob = TRUE)
table(pred1,sex.train$sex)
pred2 = predict(bn, sex.test, prob = FALSE)
table(pred2,sex.test$sex)

#貝氏網路
tan = tree.bayes(x = sex.train, training = "sex")
plot(tan)

pred1 = predict(tan, sex.train, prob = TRUE)
table(pred1, sex.train$sex)
pred2 = predict(tan, sex.test, prob = FALSE)
table(pred2, sex.test$sex)

#線型迴歸
#1.自己手動打公式
s = read.csv("D:/Shared/李御璽老師課程(資料檔)/二.使用 R 語言進行資料探勘(Data)/Data/salary.csv")
head(s,3)
x = s$Experience
y = s$Salary
beta = sum((x - mean(x)) * (y - mean(y))) / sum((x-mean(x))^2)
beta
alpha = mean(y) - beta * mean(x)
alpha

#2.套用函數 lm
s = read.csv("D:/Shared/李御璽老師課程(資料檔)/二.使用 R 語言進行資料探勘(Data)/Data/salary.csv")
head(s,3)
model = lm(Salary ~ Experience, data = s)
summary(model)
model$coefficients

results = predict(model, newdata = s)
results

sst = sum((s$Salary-mean(s$Salary))^2)
sst
ssr = sum((results-mean(s$Salary))^2)
ssr
sse = sum(residuals(model)^2)
sse
r2 = ssr /sst

n = nrow(s)
p = 1
adjusted_r2 = 1 - ((1 - r2) * (n - 1) / (n - p - 1))


residual_se = sqrt(sum(residuals(model)^2)/(length(residuals(model))-2))

#複迴歸
#1.手動
x = read.csv("D:/Shared/李御璽老師課程(資料檔)/二.使用 R 語言進行資料探勘(Data)/Data/home_prediction.csv")
head(x,3)
y = x$Value
y = as.matrix(y)
y
x = x[2:5]
x
x = cbind(intercept = 1, x)
x
x = as.matrix.data.frame(x)
x
xx = t(x) %*% x
xx
beta = solve(xx) %*% t(x) %*% y
beta
#2.套用函數
x = read.csv("D:/Shared/李御璽老師課程(資料檔)/二.使用 R 語言進行資料探勘(Data)/Data/home_prediction.csv")
x = x[-1]
head(x,3)

model = lm(x$Value ~ x$Space + x$Offices + x$Entrances + x$Age)
summary(model)

model = lm(Value ~ Space + Offices + Entrances + Age, data = x)
summary(model)

results = predict(model, newdata = x)
results

sst = sum((x$Value-mean(x$Value))^2)
sst
ssr = sum((results-mean(x$Value))^2)
ssr
sse = sum(residuals(model)^2)
sse
r2 = ssr /sst
r2

n = nrow(x)
p = 4
adjusted_r2 = 1 - ((1 - r2) * (n - 1) / (n - p - 1))

residual_se = sqrt(sse/(length(residuals(model))-2))
residual_se

s = read.csv("D:/Shared/李御璽老師課程(資料檔)/二.使用 R 語言進行資料探勘(Data)/Data/diabetes.csv")
s = s[-1]
head(s,3)

library(tree)

#目標class .是代表除了目標屬性外,其他都當輸入屬性
dia.tree = tree(class ~ ., data = s)
plot(dia.tree)
text(dia.tree)

pred = predict(dia.tree, s, type  ="class")
table(pred,s$class)

pred = cbind(pred = as.character(pred), predict(dia.tree, s, type  ="vector"))
head(pred,3)

accuracy.tree <- sum(pred==s$class)/length(s$class)

x = read.csv("D:/Shared/李御璽老師課程(資料檔)/二.使用 R 語言進行資料探勘(Data)/Data/cpu.csv")
head(x,3)
x = x[-1]

library(tree)

Perf.tree = tree(PERF ~ MYCT + MMIN + MMAX + CACH + CHMIN + CHMAX, data = x)
Perf.tree = tree(PERF ~ ., data = x)
plot(Perf.tree)
text(Perf.tree)

y_pred = predict(Perf.tree, newdata = x, type = 'vector')
y = x$PERF

sst = sum((y-mean(y))^2)
sst
ssr = sum((y_pred-mean(y))^2)
ssr
sse = sum((y_pred-y)^2)
sse
r2 = ssr /sst
r2

n = nrow(x)
p = 1
adjusted_r2 = 1 - ((1 - r2) * (n - 1) / (n - p - 1))
adjusted_r2

mae = sum(abs(y_pred-y)) / nrow(x)
mae

library(mlbench)

model <- cubist(x = x[, -7], y = x$PERF)
summary(model)

s = read.csv("C:/Users/leeys/Desktop/102train資料/RExample/Data/diabetes.csv")
head(s,3)
s = s[-1]

library(tree)

dia.tree = tree(class ~ ., data = s)
plot(dia.tree)
text(dia.tree)

pred = predict(dia.tree, s, type  ="class")
table(pred,s$class)

pred = cbind(pred = as.character(pred), predict(dia.tree, s, type  ="vector"))
head(pred,3)

tan = tree.bayes(x = sex.train, training = "sex")
plot(tan)

pred = predict(tan, sex.train, prob = FALSE)
table(pred, sex.train$sex)

#神經網路
#下載網址,儲存路徑
download.file("http://archive.ics.uci.edu/ml/machine-learning-databases/statlog/german/german.data", 
              "D:/Shared/李御璽老師課程(資料檔)/二.使用 R 語言進行資料探勘(Data)/Data/german.data")
data <- read.table("D:/Shared/李御璽老師課程(資料檔)/二.使用 R 語言進行資料探勘(Data)/Data/german.data")
head(data,6)

library("neuralnet")
#V21為目標屬性 
data <- data[,c("V2","V5","V8","V21")]
NNModelAllNum <- neuralnet(V21 ~ V2 + V5 + V8, data)
NNModelAllNum

plot(NNModelAllNum)
#compute套用神經網路模型
results = compute(NNModelAllNum, data[-ncol(data)])

#V1為類別屬性,還沒攤平 會報錯 所以下面要進行攤平
NNModel <- neuralnet(V21 ~ V1 + V2 + V5 + V8, data)

#法一 將V1攤平
dummyV1 <- model.matrix(~V1, data)
head(cbind(dummyV1, data$V1))

modelData <- model.matrix(~V1 + V2 + V5 + V8 + V21, data)
head(modelData) 

NNModel <- neuralnet(V21 ~ V1A12 + V1A13 + V1A14 + V2 + V5 + V8, modelData)
plot(NNModel)
compute(NNModel, modelData[,-c(1,ncol(modelData))])

#法二 將V1攤平
library("nnet")
dummyV12 <- class.ind(data$V1)
head(dummyV12)

#邏輯式迴歸
s = read.csv("D:/Shared/李御璽老師課程(資料檔)/二.使用 R 語言進行資料探勘(Data)/Data/diabetes.csv")
s = s[-1]
head(s,3)

lg <- glm(class ~ ., family=binomial(link='logit'), data = s)
summary(lg)

lg <- glm(class ~ preg + plas + pres + mass + pedi, family=binomial(link='logit'), data = s)
summary(lg)

pred = predict(lg, s, type  ="response")

#查看預測的機率為positive還是negative,這裡為positive
pred=cbind(pred=as.character(s$class),predict(lg, s, type  ="response"))

lg_ms<-step(lg,direction = "both", data = s)
summary(lg_ms)

library(car)
vif(lg_ms)

library(e1071)
svm.mod <- svm(class ~ ., kernel="polynomial", data=s, cost=0.1, gamma=0.4, cross=3)

y_hat = predict(svm.mod, s)
result = table(y_hat,s$class)
correct_rate = as.double(result[1,1]+result[2,2]) / (result[1,1]+result[1,2]+result[2,1]+result[2,2])
correct_rate

#隨機森林
library(randomForest)
#隨機抽樣 建立30棵樹 再進行投票
rf<-randomForest(class ~ ., data=s, ntree=30)
pre_prob<-predict(rf,s,type="prob")
pre_class<-predict(rf,s,type="class")
result = table(pre_class,s$class)
correct_rate = as.double(result[1,1]+result[2,2]) / (result[1,1]+result[1,2]+result[2,1]+result[2,2])
correct_rate

#輪廓係數
s = read.csv("D:/Shared/李御璽老師課程(資料檔)/二.使用 R 語言進行資料探勘(Data)/Data/age_income.csv")
s = s[-1]
head(s,3)

library(cluster)
#算三群 centers=3
kmd=kmeans(s,centers=3)
sil=silhouette(kmd$cluster,dist(s))
mean(sil[,'sil_width'])

result=list()
for (i in 2:5){
  kmd=kmeans(s,centers=i)
  sil=silhouette(kmd$cluster,dist(s))
  result[[paste('k=',i,sep='')]]=mean(sil[,'sil_width'])
}
result

#平均係數
mean(sil[,'sil_width'])



