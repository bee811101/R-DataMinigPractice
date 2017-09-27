
testone <- read.table("D:/Shared/M04A-8-mon.csv",header = FALSE ,sep = ",")
names(testone) <- c("RecordT","start","end","type","time","number")
library(dplyr)
temmm <- c("01F0928S","01F0880S","01F0750S","01F0681S","01F0664S","01F0633S","01F0578S","01F0557S","01F0532S","01F0509S","01F0467S","01F0413S","01F0376S","01F0339S","01F0293S","01F0264S")

www <- local({
  testone[grep("^01",testone$end),] %>%
    filter(type=="31") 
})
x <- data.frame(temmm)

names(x) <- "start"

inn <- inner_join(www,x)
inn <- inn[grep("S$",inn$end),]
inn <- inn[grep("[345].$",inn$RecordT),]
x <- local({
  inn%>%
    filter(start=="01F0264S") %>%
     arrange(start) %>%
      mutate(date=substring(RecordT,1,10),dateT=substring(RecordT,12,16)) %>% 
       select(date,dateT,time,number)
})
#pivot資料
library(reshape2)
xt1 <- dcast(x,date~dateT ,value.var = "time")
xt2 <- dcast(x,date~dateT,value.var = "number")
finalT <- left_join(x=xt1,y=xt2,by="date")
finalT
      