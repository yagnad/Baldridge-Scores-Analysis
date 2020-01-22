rm(list=ls())
library(rio)
library(moments)
df = import("baldridge2011.xlsx")
setwd("C:/Users/yagna/Documents/R/R workings")
str(df)
df$ccrtotal <- as.numeric(df$ccrtotal)

library(stargazer)
library(ggplot2)

hist(df$iirtotal,xlim=c(0,900),col = 'red',main = "Histogram of IIR Total")
hist(df$ccrtotal,xlim=c(0,900),col = 'blue',main = "Histogram of CCR Total")

#1a
stargazer(df[c("iirtotal","ccrtotal")],type = "text")
median(df$iirtotal,na.rm=TRUE)
median(df$ccrtotal,na.rm =TRUE)

#function for Mode
Mode = function(x){
  ta = table(x)
  tam = max(ta)
  if (all(ta == tam))
    mod = NA
  else
    if(is.numeric(x))
      mod = as.numeric(names(ta)[ta == tam])
  else
    mod = names(ta)[ta == tam]
  return(mod)
}

Mode(df$iirtotal)
Mode(df$ccrtotal)

#1b

stargazer(subset(df[c("ccrtotal","iirtotal")],df$sector==1),
          title="Manufacturing Sector", type = "text", 
          digits=2, median = TRUE, omit.summary.stat = c("p25", "p75","N"))
stargazer(subset(df[c("ccrtotal","iirtotal")], df$sector==2),
          title="Service Sector", type = "text",
          digits=2, median = TRUE, omit.summary.stat = c("p25", "p75","N"))
stargazer(subset(df[c("ccrtotal","iirtotal")], df$sector==3),
          title="Small Sector", type = "text", 
          digits=2, median = TRUE, omit.summary.stat = c("p25", "p75","N"))
stargazer(subset(df[c("ccrtotal","iirtotal")], df$sector==4),
          title="Education Sector", type = "text", 
          digits=2, median = TRUE, omit.summary.stat = c("p25", "p75","N"))
stargazer(subset(df[c("ccrtotal","iirtotal")], df$sector==5),
          title="Health Sector", type = "text", 
          digits=2, median = TRUE, omit.summary.stat = c("p25", "p75","N"))
stargazer(subset(df[c("ccrtotal","iirtotal")], df$sector==6),
          title="Non-Profit Sector", type = "text", 
          digits=2, median = TRUE, omit.summary.stat = c("p25", "p75","N"))

#1c

ggplot(df, aes(y=df$ccrtotal)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 21, 
               outlier.size = 2)

min(df$ccrtotal, na.rm = true)
which(df$ccrtotal == 185)
df = df[-506,]

stargazer(df[c("iirtotal","ccrtotal")],type = "text")

#1d
plot(df$icat4total,df$ccat7total, xlab = "ICat4Total",ylab = "CCat7Total",pch=19, col=c("Red","blue"))
df$ccat7total <- as.numeric(df$ccat7total)
cor(df[c("icat4total","ccat7total")])  #put vars of interest in double quotes
cor(x=df$icat4total,y=df$ccat7total,use="complete.obs")

#1E

tbl = as.data.frame(sort(table(df$permanentid), decreasing = TRUE))
stargazer(tbl, type = "text", title="Descriptive statistics", digits=2, summary = FALSE)


#1F

tbl.sub = subset(tbl,tbl$Freq >=6)
tbl.sub
subset(df[c("ccrtotal","iirtotal")],df$sector==1)


#2a


a = subset(df[c("icat4total","icat7total")],df$sector==5)

a$icat4total <- as.numeric(a$icat4total)
a$icat7total <- as.numeric(a$icat7total)

hist(a$icat4total,xlim=c(0,350),col = 'red',main = "Histogram of ICAT4Total & ICAT7Total")
hist(a$icat7total,xlim=c(0,350), add=T,col=rgb(0, 1, 0, 0.5))

a = subset(df[c("icat4total","ccat7total")],df$sector==5)

a$icat4total <- as.numeric(a$icat4total)
a$ccat7total <- as.numeric(a$ccat7total)

hist(a$icat4total,xlim=c(0,300),col = 'red',main = "Histogram of ICAT4Total & ICAT7Total")
hist(a$ccat7total, add=T,col=rgb(0, 1, 0, 0.5))

#2B

b = subset(df[c("icat4total","ccat7total","year")],df$sector==1)

plot(b$icat4total,b$ccat7total, main = "Plot of MFG sector")

#2C

plot(v,type,col,xlab,ylab)
plot(mean(df$iirtotal), ylim = year, type = 'o', col = "red")

# Libraries
library(ggplot2)

# create data
xValue <- tapply(df$iirtotal, sector , mean)
xValue 
yValue <- tapply(df$iirtotal, year, mean)
yValue
xy <- data.frame(xValue,yValue)

df = transform(df, iirtotal = ifelse(is.na(iirtotal), mean(iirtotal, na.rm=TRUE), iirtotal))

xy <- ggplot(df,aes(x = yValue, y = mean(iirtotal), group = sector)) +
  geom_line()


xy
# Plot
ggplot(xy, aes(x=xValue, y=yValue)) +
  geom_line()+
  ggtitle("mean of iirtotal")

#2E

mfg = (subset(df[c("icat1total","icat2total","icat3total","icat4total","icat5total","icat6total", "icat7total")],df$sector==1))
mfg

boxplot(mfg, labels(mfg))

ggplot(mfg, aes(x = frequency(mfg), y= colnames(mfg)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 21, 
               outlier.size = 2) +
              stat_summary(
                aes_(label = stat(mfg)),
                geom = "text",
                fun.y = function(y) { o <- boxplot.stats(y)$out; if(length(o) == 0) NA else o },
                hjust = -1
              ))

#2F
library(lattice)   # Load library into memory
b$year <- as.numeric(b$year)

b$time_period <- ifelse(b$year <= 1994,0,1)
b$time_period <- ifelse(b$year > 1994,1,2)
b$time_period <- ifelse(b$year >= 1999,2,5)


b$time_period <- ifelse(b$year <= 1994, 0
       ifelse(1995 <= b$year <= 1998,1, 
              ifelse(b$year >= 1999,2,1)))

b$time_period

#egs
xyplot(b$icat4total~b$ccat7total , col=as.numeric(time_period))   #col is for color of tick marks and labels?
xyplot(d$Price~d$SqFt | d$Brick, col=as.numeric(d$Brick))  #col is for color of tick marks and labels?

xyplot(d$Price~d$SqFt | d$Brick, col=as.numeric(d$Brick), cex=d$Offers)  #circle size proportional to offers

xyplot(d$Price~d$SqFt|d$Brick,groups=d$Brick)  #lattice library, a different way
xyplot(d$Price~d$SqFt|d$Neighborhood,groups=d$Brick)  #lattice library

plot(d$SqFt, d$Price, col=c("red","blue")[d$Brick])   #scatterplot showing two groups by color
legend(x="topright", legend=levels(d$Brick), col=c("red", "blue"), pch=1)    #adds legend on topright






#2G

stargazer(subset(df[c("icat1total","icat2total","icat3total","icat4total","icat5total","icat6total", "icat7total")]),
          title="SUMMARY STATISTICS", type = "text", 
          digits=2, median = TRUE, omit.summary.stat = c("p25", "p75"))



#3a
apply.ci(df$icat7total)

stargazer(df[c("iirtotal")],type = "text")


