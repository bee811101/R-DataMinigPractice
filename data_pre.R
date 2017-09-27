german = read.csv("D:/Shared/李御璽老師課程(資料檔)/二.使用 R 語言進行資料探勘(Data)/Data/german.csv")
head(german,3)

nrow(german)
ncol(german)
length(german)

a = sapply(german, class)
names(a[1])
a[1]

#german$num_dependents <- NULL
#german_del_num_dependents <- NULL


summary(german[3])
sd(german$duration, na.rm = TRUE)
summary(german$purpose)
summary(german[5])

length(unique(german$checking_status))
length(unique(german$ID))
length(unique(german$num_dependents))
summary(german$num_dependents)
summary(german$purpose)

german$installment_commitment <- as.factor(german$installment_commitment)
german$residence_since <- as.factor(german$residence_since)
german$existing_credits <- as.factor(german$existing_credits)
german$num_dependents <- as.factor(german$num_dependents)
german$class <- as.factor(german$class)

table(german$installment_commitment)
table(german$residence_since)
table(german$existing_credits)
table(german$num_dependents)
table(german$class)

install.packages('ggplot2')
library(ggplot2)

ggplot(german, aes(checking_status)) + 
  geom_bar() +
  coord_flip() 

ggplot(german, aes(age)) +
  geom_bar()

ggplot(german, aes(checking_status)) + 
  geom_bar(aes(fill=class)) +
  coord_flip() 

ggplot(german, aes(checking_status)) +
  geom_bar(aes(fill=class))

ggplot(german, aes(checking_status)) + 
  geom_bar(aes(fill=class), position = position_fill(reverse = TRUE)) +
  coord_flip() 

ggplot(german, aes(age)) +
  geom_bar(aes(fill=class), position = position_fill())

ggplot(german, aes(age)) +
  geom_bar(aes(fill=class), position = position_fill(reverse = TRUE))

ggplot(german, aes(age)) +
  geom_histogram(aes(fill=class), position = 'fill', bins = 20)

#checking_status_map <- c("A11"=0.21, "A12"=0.4, "A13"=0.6, "A14"=0.8)
#german$checking_status <- checking_status_map[as.character(german$checking_status)]
#german$checking_status <- as.numeric(german$checking_status)
#german$num_dependents[german$num_dependents == '1'] <- '0.21'
#german$num_dependents[german$num_dependents == '2'] <- '0.79'
#german$num_dependents <- as.numeric(german$num_dependents)

ggplot(german, aes(num_dependents)) + 
  geom_bar(aes(fill=class), position = position_fill(reverse = TRUE)) +
  coord_flip() 

german$num_dependents <- NULL

german[german$purpose == 'X' | german$purpose == '', 'purpose'] <- NA
table(german$purpose)

german$purpose<-factor(german$purpose, levels = c('A40','A41','A410','A42','A43','A44','A45','A46','A48','A49')) 
summary(german$purpose)

ggplot(german, aes(purpose)) + 
  geom_bar(aes(fill=class), position = position_fill(reverse = TRUE)) +
  coord_flip() 

sorted_age <- sort(german$age)
quantile(german$age, c(0, 0.05, 0.95, 1.0))

age_lb <- mean(german$age, na.rm = TRUE) - 3 * sd(german$age, na.rm = TRUE)
age_ub <- mean(german$age, na.rm = TRUE) + 3 * sd(german$age, na.rm = TRUE)
age_lb <- floor(age_lb)
age_ub <- floor(age_ub)
german$age[german$age < age_lb] <- age_lb
german$age[german$age > age_ub] <- age_ub

age_percentile <- quantile(german$age, c(0.25, 0.75))
age_iqr <- age_percentile[2] - age_percentile[1]
age_lb <- age_percentile[1] - 1.5 * age_iqr
age_ub <- age_percentile[2] + 1.5 * age_iqr
german$age[german$age < age_lb] <- age_lb
german$age[german$age > age_ub] <- age_ub

#NULL 

log2_age <- log2(german$age)
log10_age <- log10(german$age)
logb_age <- logb(german$age)

#資料有空值
#查看欄位是否完整,只取晚整的資料
complete.cases(german)
german.all <- german[complete.cases(german), ]
nrow(german.all)


german$purpose_ind <- is.na(german$purpose)
german$purpose_ind <- NULL

sum(is.na(german$purpose))
tmp <- german$purpose
german$purpose<-factor(german$purpose, levels = c('A40','A41','A410','A42','A43','A44','A45','A46','A48','A49','unknown')) 
german[is.na(german$purpose),]$purpose <- 'unknown'
summary(german$purpose)
german$purpose <- tmp

tmp <- german$purpose
summary(german$purpose)

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

german[is.na(german$purpose),]$purpose <- getmode(german$purpose)
summary(german$purpose)
german$purpose <- tmp

#用knn建模,填補空值
install.packages('DMwR')
library(DMwR)

imputedData <- knnImputation(german, k=10, scale = T)
summary(imputedData$purpose)

install.packages('mice')
library(mice)

imputedData <- mice(german,
                  m = 3,           # ????????????被填???好?????????表
                  maxit = 50,      # max iteration
                  method = "cart", # 使用CART決???樹，進???遺漏值???測
                  seed = 188)      # set.seed()，令??????每次???一???

imputedData <- complete(imputedData, 1)
summary(imputedData$purpose)
summary(imputedData$duration)

drug = read.csv("D:/Shared/李御璽老師課程(資料檔)/二.使用 R 語言進行資料探勘(Data)/Data/DRUG1n.csv")

library(ggplot2)
ggplot(drug, aes(Na, K)) +
  geom_point(aes(color=Drug)) + 
  scale_fill_manual(values=c("red", "blue"))

drug$Na_to_K <- drug$Na / drug$K
drug$Na <- NULL
drug$K <- NULL

library(MASS)
tbl <- table(german$checking_status, german$class)
#卡方檢定
chisq.test(tbl)

#glm不用將factor轉為numeric,可以直接跑
summary(glm(class ~ age, family = binomial,data=german))

#先將factor轉為numeric,才能跑anova
german$class <- as.numeric(german$class)
german$class <- german$class - 1
aov(class ~ age, data=german)
anova(lm(class ~ age, data=german))

install.packages('corrplot')
library(corrplot)
integer_cols <- names(german)[sapply(german, is.numeric)]
integer_cols
M <- cor(german[, integer_cols], use = "pairwise.complete.obs")
corrplot(M, method="number")

german$class <- as.numeric(german$class)

german$class <- german$class - 1
fullmodel <- glm(class ~ . - ID, family = binomial, data = german)
backwards = step(fullmodel)

install.packages("ROCR")



