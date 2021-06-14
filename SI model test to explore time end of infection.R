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
  
  while(I<.9999){
  
  ds<- -beta*I*S
  di<- beta*I*S
  

  
  I<-round((I + di),4)
  S <-round((S + ds),4)
  
  
  t<-t+1
  
  simodeldf<-rbind(simodeldf,data.frame(Susceptible=S,Infected=I,Time=t))
  } 

  
tablemelt<-melt(simodeldf,id.vars="Time")

ggplot(tablemelt)+geom_line(aes(x=Time,y=value,group=variable))


##############################Testing the integral equation###################################################

inteq<-function(beta,sus,time){
  
  I<-exp(beta*sus*time)
  print(I)
}

inteq(.5,simodeldf[20,1],simodeldf[20,3])