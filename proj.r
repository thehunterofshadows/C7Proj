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
  geom_point(aes(col=as.factor(am)),size=4)+
  labs(subtitle = "MPG to Weight with Trans",
       x="Weight in Tons",
       y="Miles per Gallon",
       title="Plot")+
  scale_colour_discrete(name="Trans",
                        labels=c("Manual","Auto"))

  
#vif fit of each variable
v<-vif(fitAll)