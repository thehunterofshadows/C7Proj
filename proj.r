#data and libraries
library(stats)
library(ggplot2)
library(car)
library(corrplot)
#library(corrgram)
data(mtcars)
#which items should be factors?  
mtcars<-mtcars
mtcars$am<-as.logical(mtcars$am)
#V2mtcars$cyl<-as.factor(V2mtcars$cyl)
#mtcars$cyl.4<-as.logical(FALSE)  #There is a better way to do this
#mtcars$cyl.4[cyl=4]<-as.logical(TRUE)
mtcars$cyl.6<-as.logical(FALSE)  #There is a better way to do this
mtcars$cyl.6[ which(mtcars$cyl==6)]<-as.logical(TRUE)
mtcars$cyl.8<-as.logical(FALSE)  #There is a better way to do this
mtcars$cyl.8[ which(mtcars$cyl==8)]<-as.logical(TRUE)

#plot corrlatoin of all the data points
corrplot(cor(mtcars),method="square")

#models
fit1<-lm(mpg~am+wt,mtcars)
fit2<-lm(mpg~am+wt+gear,mtcars)
fit3<-lm(mpg~am+wt+gear+disp,mtcars)
fit4<-lm(mpg~am+wt+gear+disp+drat,mtcars)
fit5<-lm(mpg~am+wt+gear+disp+drat+carb,mtcars)
fit6<-lm(mpg~am+wt+gear+disp+drat+carb+qsec,mtcars)
fit7<-lm(mpg~am+wt+gear+disp+drat+carb+qsec+vs,mtcars)
fitAll<-lm(mpg~.,mtcars)
fit9<-lm(mpg~am+wt+disp+carb,mtcars)
anova1<-anova(fit1, fit2,fit3,fit4,fit5, fit6, fit7,fit9,fitAll)

#compare slopes of auto to manual - We can see here that while manual has better mpg,
# manual is also in smaller cars.
g<-ggplot(mtcars, aes(wt, mpg))+
  geom_point(aes(col=as.factor(am), size=disp))+
  labs(subtitle = "MPG to Weight with Trans and Displacement",
       x="Weight in Tons",
       y="Miles per Gallon",
       title="Plot")+
  scale_colour_discrete(name="Trans",
                       labels=c("Manual","Auto"))+
  scale_size_continuous(name="Engine Displacement in CI")
  

#Plot out the mpg with cyl
cylPlot<-ggplot(mtcars, aes(as.factor(cyl), mpg))+
  geom_point(aes(col=as.factor(cyl)))+
  labs(subtitle = "MPG to # of Cylinders",
       x="# of Cylinders",
       y="Miles per Gallon",
       title="Plot")+
  scale_colour_discrete(name="Cylinders")

# manual is also in smaller cars.
g2<-ggplot(mtcars, aes(wt, mpg))+
  geom_point(aes(col=as.factor(am)),size=5)+
  labs(subtitle = "MPG to Weight with Trans",
       x="Weight in Tons",
       y="Miles per Gallon",
       title="Plot")+
  scale_colour_discrete(name="Trans",
                        labels=c("Manual","Auto"))

#Plot the residuals
## for mpg to wt - not realy what you were supose to do
res=ggplot(data.frame(x=mtcars$wt, y=resid(lm(mpg~wt,mtcars))),
           aes(x=x,y=y))+
  geom_hline(yintercept = 0,size=2)+
  geom_point(size=7, colour="black", alpha=0.4)+
  geom_point(size=5,colour="red",alpha=0.4)+
  xlab("Weight")+ylab("Residual")
##add in colors for trans
res2=ggplot(data.frame(am=mtcars$am,x=mtcars$wt, y=resid(lm(mpg~wt,mtcars))),
           aes(x=x,y=y))+
  geom_hline(yintercept = 0,size=2)+
  geom_point(size=7, colour="black", alpha=0.4)+
  geom_point(size=5,aes(col=as.factor(am)),alpha=0.4)+
  xlab("Weight")+ylab("Residual")

resMPG2AM=ggplot(data.frame(x=as.factor(mtcars$am), y=resid(lm(mpg~as.factor(am),mtcars))),
           aes(x=x,y=y))+
  geom_hline(yintercept = 0,size=2)+
  geom_point(size=7, colour="black", alpha=0.4)+
  geom_point(size=5,colour="red",alpha=0.4)+
  xlab("Trans")+ylab("Residual")

res.am.wt=ggplot(data.frame(am=as.factor(mtcars$am),x=as.factor(mtcars$wt), y=resid(lm(mpg~wt+as.factor(am),mtcars))),
                 aes(x=x,y=y))+
  geom_hline(yintercept = 0,size=2)+
  geom_point(size=7, colour="black", alpha=0.4)+
  geom_point(size=5,alpha=0.4,aes(col=am))+
  xlab("Weight in Tons")+ylab("Residual")

#Sports car look
#View(subset(mtcars, wt>=3 & wt<=4))
mtcars$disp2wt=(mtcars$disp/mtcars$wt)
#View(mtcars[order(disp)])
  
#vif fit of each variable
v<-vif(fitAll)