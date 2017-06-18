#Load packages
install.packages("ggplot2")
install.packages("psych")
install.packages("scales")
install.packages("gridExtra")
library(psych)
library(ggplot2)
library(scales)
library(gridExtra)
require(grid)

#Descriptive statistics
summary(data)
describe(data[,1:21])

#Generate plots for numerical variables:
##gnumplots function (automatically generates plots for multiple numerical variables)
gnumplots<-function(data,varlist){
  plots.mtx<-matrix(list(),nrow=length(varlist),ncol=3)
  for(i in 1:length(varlist)){
    plots.mtx[[i,1]]<-ggplot(data,aes(x=get(varlist[[i]])))+
      geom_histogram(bins = 30)+
      xlab(varlist[[i]])
    plots.mtx[[i,2]]<-ggplot(data,aes(x=get(varlist[[i]]),fill=y,colour=y))+
      geom_density(alpha=0.25)+
      xlab(varlist[[i]])+
      scale_y_continuous(labels = percent)
    quant.values<-quantile(x = data[,varlist[[i]]], probs = seq(0,1,0.1))
    plotsrc<-data.frame(
      quant=c(1:10,1:10),
      y=rep(c("yes","no"),each=10),
      count=vector(mode = "integer",length = 20),
      freq=vector(mode = "double",length = 20))
    for (j in 1:20) {
      if (j %in% c(1,11)) {
        plotsrc$count[j]<-sum(data$y==plotsrc$y[j] & data[,varlist[[i]]]<=quant.values[plotsrc$quant[j]+1])
      } else {
        plotsrc$count[j]<-sum(data$y==plotsrc$y[j] & data[,varlist[[i]]]<=quant.values[plotsrc$quant[j]+1])-
          sum(data$y==plotsrc$y[j] & data[,varlist[[i]]]<=quant.values[plotsrc$quant[j-1]+1])
      }
    }
    for (j in 1:20) {
      if (sum(plotsrc$count[plotsrc$quant==plotsrc$quant[j]])>0) {
        plotsrc$freq[j]<-plotsrc$count[j]/sum(plotsrc$count[plotsrc$quant==plotsrc$quant[j]])
      } else {
        plotsrc$freq[j]<-plotsrc$freq[j-1]
      }
    }
    plots.mtx[[i,3]]<-ggplot(data=plotsrc,aes(x=quant,y=freq,fill=y))+
      geom_bar(stat="identity")+
      xlab(paste(varlist[[i]],"(quantiles)",sep = " "))+
      ylab("frequency")+
      scale_x_continuous(breaks = seq(1,10,1),labels=quant.values[2:11])+
      scale_y_continuous(labels=percent)+
      geom_label(aes(label=percent(freq)),
                 size=3,
                 position = position_stack(vjust = 0.5),
                 show.legend = FALSE)+
      guides(fill=guide_legend(title="y"))
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(2, 2)))
    print(plots.mtx[[i,1]],
          vp=viewport(layout.pos.row = 1, layout.pos.col = 1)
    )
    print(plots.mtx[[i,2]],
          vp=viewport(layout.pos.row = 1, layout.pos.col = 2)
    )
    print(plots.mtx[[i,3]],
          vp=viewport(layout.pos.row = 2, layout.pos.col = 1:2)
    )
  }
}
##Create the variable list and use gnumplots function 
numvarlist<-list(
  "age",
  "duration",
  "campaign",
  "pdaysinv",
  "previous",
  "emp.var.rate",
  "cons.price.idx",
  "cons.conf.idx",
  "euribor3m",
  "nr.employed")
gnumplots(data,numvarlist)

#Generate plots for categorical variables
##gcatplots function (automatically generates plots for multiple categorical variables)
gcatplots<-function(data,varlist){
  plots.mtx<-matrix(list(),nrow=length(varlist),ncol=3)
  for (i in 1:length(varlist)){
    plots.mtx[[i,1]]<-ggplot(data,aes(x = get(varlist[[i]])))+
      geom_bar()+
      xlab(varlist[[i]])
    cont<-as.data.frame(table(data[,varlist[[i]]],data$y))
    freqy<-cont[cont$Var2=="yes",3]/sum(cont[cont$Var2=="yes",3])
    freqn<-cont[cont$Var2=="no",3]/sum(cont[cont$Var2=="no",3])
    dir<-names(table(data[,varlist[[i]]]))
    dfy<-data.frame(dir=dir,y=rep("yes",length(dir)),freq=freqy)
    dfn<-data.frame(dir=dir,y=rep("no",length(dir)),freq=freqn)
    df<-rbind(dfy,dfn)
    plots.mtx[[i,2]]<-ggplot(data=df,aes(x=y,y=freq,fill=dir))+
      geom_bar(stat="identity")+
      ylab("frequency")+
      scale_y_continuous(labels = percent)+
      guides(fill=guide_legend(title=varlist[[i]]))
    len<-nrow(cont)
    Freqk<-vector(mode="integer",length=len)
    for (k in 1:(len/2)){
      Freqk[k]<-cont[k,3]/(cont[k,3]+cont[k+len/2,3])
      Freqk[k+len/2]<-cont[k+len/2,3]/(cont[k,3]+cont[k+len/2,3])
    }
    cont$Freq<-Freqk
    plots.mtx[[i,3]]<-ggplot(data=cont,aes(x=Var1,y=Freq,fill=Var2))+
      geom_bar(stat="identity")+
      xlab(varlist[[i]])+
      ylab("frequency")+
      scale_y_continuous(labels = percent)+
      geom_label(aes(label=percent(Freq)),
                 size=3,
                 position = position_stack(vjust = 0.5),
                 show.legend = FALSE)+
      guides(fill=guide_legend(title="y"))
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(2, 2)))
    print(plots.mtx[[i,1]],
          vp=viewport(layout.pos.row = 1, layout.pos.col = 1)
    )
    print(plots.mtx[[i,2]],
          vp=viewport(layout.pos.row = 1, layout.pos.col = 2)
    )
    print(plots.mtx[[i,3]],
          vp=viewport(layout.pos.row = 2, layout.pos.col = 1:2)
    )
  }
}
##Create the variable list and use gcatplots function 
catvarlist<-list(
  "marital",
  "job",
  "education",
  "default",
  "housing",
  "loan",
  "contact",
  "month",
  "day_of_week",
  "poutcome")
gcatplots(data,catvarlist)