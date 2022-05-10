library(olsrr)
library(janitor)
library(leaps)
library(knitr)

df <- read.csv2("İstanbul_İlçeler.csv", sep=";")
df <- subset(df, select = -c(name) )

df[, c(3:10,15)] <- sapply(df[, c(3:10,15)], as.numeric)
df <- as.data.frame(scale(df))

formula <- formula(paste("grophar ~ ", 
                         paste(names(df[c(1:4, 11:19)]), collapse=" + ")))

allpossreg <- regsubsets(formula,nbest=40,data=df)
aprout <- summary(allpossreg)

APRtable=with(aprout,round(cbind(which,rsq,adjr2),13))
APRtable=data.frame(APRtable,check.rows = F,row.names = NULL)
APRtable$ppri=rowSums(APRtable[,1:4])
kable(APRtable)

require(ggplot2)
ggplot(APRtable, aes(x=ppri-1, y=rsq)) +
  geom_point(shape=1,size=3)+   
  scale_y_continuous(breaks = seq(0.5, 1, by = 0.05)) +
  scale_x_continuous(breaks = seq(0, 3, by = 1))+
  theme_bw()+labs(x = "R-squared")+ 
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=14,face="bold"))

ggplot(APRtable, aes(x=ppri-1, y=adjr2)) +
  geom_point(shape=1,size=3)+   
  scale_y_continuous(breaks = seq(0.5, 1, by = 0.05)) +
  scale_x_continuous(breaks = seq(0, 3, by = 1))+
  theme_bw()+labs(x = "Adjusted R-squared")+ 
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=14,face="bold"))


library("writexl")
write_xlsx(APRtable,"grocpha.xlsx")
