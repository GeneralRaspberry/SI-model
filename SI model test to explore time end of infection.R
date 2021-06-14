library("dplyr")
library("ggplot2")
library("reshape2")

  
infected<-1
hosts<-1000
t<-0
I<-infected/hosts
S<-1-I
beta<-.5


  simodeldf<-data.frame(Susceptible=S,Infected=I,Time=t)
  
  while(I<1){
  if(I<1){
  
  ds<- -beta*I*S
  di<- beta*I*S
  

  
  I<-I + di
  S <- S + ds
  
  
  t<-t+1
  
  simodeldf<-rbind(simodeldf,data.frame(Susceptible=S,Infected=I,Time=t))
  } else {
    break
  }
  }
  
tablemelt<-melt(simodeldf,id.vars="Time")

ggplot(tablemelt)+geom_line(aes(x=Time,y=value,group=variable))