#Load packages
install.packages("ggplot2")
install.packages("psych")
library(psych)
library(ggplot2)

#Descriptive statistics
summary(data)
describe(data[,1:21])

#Generate plots for numerical variables:
##gnumplots function (automatically generates plots for multiple numerical variables)
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
##Create the variable list and use gnumplots function 
numvarlist<-list("age","duration","campaign","pdaysinv","previous","emp.var.rate","cons.price.idx","cons.conf.idx","euribor3m","nr.employed")
gnumplots(data,numvarlist)

#Generate plots for categorical variables
##gcatplots function (automatically generates plots for multiple categorical variables)
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
  rm(cont,dfy,dfn,df,dir,Freq,freqn,freqy,Freqk,len)
}
##Create the variable list and use gcatplots function 
catvarlist<-list("marital","education","default","housing","loan","contact","month","day_of_week","poutcome")
gcatplots(data,catvarlist)