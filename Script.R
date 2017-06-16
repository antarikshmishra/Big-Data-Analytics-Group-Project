#Load data
setwd("~/Library/Mobile Documents/com~apple~CloudDocs/Études (ESSEC)/MSc - Big Data Analytics/Case")
data<-read.table("Data.csv",header=TRUE,sep=";",dec=".")

#Load packages
library(psych)
library(ggplot2)

#Descriptive statistics
summary(data)
describe(data[,1:21])

#Plot numerical variables
ggplot(data,aes(x=age))+
  geom_histogram(center = 2.5,binwidth = 5)+
  scale_x_continuous(breaks = seq(0,100,by=10),limits=c(0,100),minor_breaks = NULL)+
  scale_y_continuous(breaks = seq(0,10000,by=2000),limits=c(0,10000),minor_breaks = NULL)+
  xlab("Age")+
  ylab(NULL)+
  theme_minimal()
ggplot(data,aes(x="x",y=age))+
  geom_boxplot()+
  scale_x_discrete(labels=NULL)+
  scale_y_continuous(breaks = seq(0,100,by=10),limits=c(0,100),minor_breaks = NULL)+
  xlab("Age")+
  ylab(NULL)+
  theme_minimal()
ggplot(data,aes(x=y,y=age))+
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75))+
  scale_x_discrete(labels=c("Yes","No"))+
  scale_y_continuous(breaks = seq(0,100,by=10),limits=c(0,100),minor_breaks = NULL)+
  xlab("Has the client subscribed a term deposit?")+
  ylab("Age")+
  theme_minimal()
ggplot(data,aes(x=y,y=age))+
  geom_violin()+
  geom_boxplot(width=0.2,position="identity")+
  scale_x_discrete(labels=c("Yes","No"))+
  scale_y_continuous(breaks = seq(0,100,by=10),limits=c(0,100),minor_breaks = NULL)+
  xlab("Has the client subscribed a term deposit?")+
  ylab("Age")+
  theme_minimal()

#Plot categorical variables
ggplot(data,aes(x = y,fill=marital))+
  geom_bar()
ggplot(data,aes(x = y, fill = marital))+
  geom_bar(position=position_stack())

cont<-as.data.frame(table(data$marital,data$y))
freqy<-cont[cont$Var2=="yes",3]/sum(cont[cont$Var2=="yes",3])
freqn<-cont[cont$Var2=="no",3]/sum(cont[cont$Var2=="no",3])
dir<-names(table(data$marital))
dfy<-data.frame(dir=dir,y=rep("Yes",length(dir)),freq=freqy)
dfn<-data.frame(dir=dir,y=rep("No",length(dir)),freq=freqn)
df<-rbind(dfy,dfn)
ggplot(data=df,aes(x=y,y=freq,fill=dir))+
  geom_bar(stat="identity")+
  xlab("Has the client subscribed a term deposit?")+
  ylab("Frequency")+
  guides(fill=guide_legend(title="Categories"))

cont<-as.data.frame(table(data$marital,data$y))
len<-nrow(cont)
Freqk<-vector(mode="integer",length=len)
for (k in 1:(len/2)){
  Freqk[k]<-cont[k,3]/(cont[k,3]+cont[k+len/2,3])
  Freqk[k+len/2]<-cont[k+len/2,3]/(cont[k,3]+cont[k+len/2,3])
}
cont$Freq<-Freqk
dir<-names(table(data$marital))
ggplot(data=cont,aes(x=Var1,y=Freq,fill=Var2))+
  geom_bar(stat="identity")+
  xlab("marital")+
  ylab("Frequency")+
  guides(fill=guide_legend(title="y"))

#Generate plots for numerical values
gnumplots<-function(plotsdata,varlist){
  for(i in 1:length(varlist)){
    print(ggplot(plotsdata,aes(x=get(varlist[[i]])))+
            geom_histogram()+
            xlab(varlist[[i]])+
            ylab(NULL))
    print(ggplot(plotsdata,aes(x=y,y=get(varlist[[i]])))+
            geom_violin(aes(fill=y))+
            geom_boxplot(width=0.1,outlier.alpha=0)+
            scale_x_discrete(labels=c("No","Yes"))+
            xlab("Has the client subscribed a term deposit?")+
            ylab(varlist[[i]])+
            theme(legend.position="none")
          )
  }
}
numvarlist<-list("age","duration","campaign","pdaysinv","previous","emp.var.rate","cons.price.idx","cons.conf.idx","euribor3m","nr.employed")
gnumplots(data,numvarlist)

#propcatplot
propcatplot <- function(var1, var2, titleplot){
  require(ggplot2)
  levVar1 <- length(levels(var1))
  levVar2 <- length(levels(var2))
  
  jointTable <- prop.table(table(var1, var2))
  plotData <- as.data.frame(jointTable)
  plotData$marginVar1 <- prop.table(table(var1))
  plotData$var2Height <- plotData$Freq / plotData$marginVar1
  plotData$var1Center <- c(0, cumsum(plotData$marginVar1)[1:levVar1 -1]) +
    plotData$marginVar1 / 2
  
  ggplot(plotData, aes(var1Center, var2Height)) +
    geom_bar(stat = "identity", aes(width = marginVar1, fill = var2), col = "Black") +
    geom_text(aes(label = as.character(var1), x = var1Center, y = 1.05))+
    xlab(NULL)+
    ylab(NULL)+
    labs(title=titleplot)+
    guides(fill=guide_legend(title=NULL))
}

#Generate plots for categorical values
gcatplots<-function(data,varlist){
  for (i in 1:length(varlist)){
    print(
      ggplot(data,aes(x = get(varlist[[i]])))+
        geom_bar()+
        xlab(varlist[[i]])
    )
    cont<-as.data.frame(table(data[,varlist[[i]]],data$y))
    freqy<-cont[cont$Var2=="yes",3]/sum(cont[cont$Var2=="yes",3])
    freqn<-cont[cont$Var2=="no",3]/sum(cont[cont$Var2=="no",3])
    dir<-names(table(data[,varlist[[i]]]))
    dfy<-data.frame(dir=dir,y=rep("Yes",length(dir)),freq=freqy)
    dfn<-data.frame(dir=dir,y=rep("No",length(dir)),freq=freqn)
    df<-rbind(dfy,dfn)
    print(
      ggplot(data=df,aes(x=y,y=freq,fill=dir))+
        geom_bar(stat="identity")+
        xlab("Has the client subscribed a term deposit?")+
        ylab("Frequency")+
        guides(fill=guide_legend(title=varlist[[i]]))
    )
    len<-nrow(cont)
    Freqk<-vector(mode="integer",length=len)
    for (k in 1:(len/2)){
      Freqk[k]<-cont[k,3]/(cont[k,3]+cont[k+len/2,3])
      Freqk[k+len/2]<-cont[k+len/2,3]/(cont[k,3]+cont[k+len/2,3])
    }
    cont$Freq<-Freqk
    print(
      ggplot(data=cont,aes(x=Var1,y=Freq,fill=Var2))+
        geom_bar(stat="identity")+
        xlab(varlist[[i]])+
        ylab("Frequency")+
        guides(fill=guide_legend(title="y"))
    )
  }
}

catvarlist<-list("marital","education","default","housing","loan","contact","month","day_of_week","poutcome")
gcatplots(data,catvarlist)


#Correlation matrix
install.packages("corrplot")
library(corrplot)
corrplot(cor(bank_additional_full), method="shade", shade.col=NA, tl.col="black", tl.srt=45, addCoef.col="black",type = "lower")

#Transform pdays
pdaysinv<-1/(data$pdays+1)
pdaysinv[pdaysinv == 1/1000] <- 0
describe(pdaysinv)
data<-cbind(data,pdaysinv)