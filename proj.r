data(mtcars)
mtcars$am<-as.factor(mtcars$am)

#models
fit1<-lm(mpg~am+wt,mtcars)
fit2<-lm(mpg~am+wt+gear,mtcars)
fit3<-lm(mpg~am+wt+gear+disp,mtcars)
fit4<-lm(mpg~am+wt+gear+disp+drat,mtcars)
fit5<-lm(mpg~am+wt+gear+disp+drat+carb,mtcars)
fit6<-lm(mpg~am+wt+gear+disp+drat+carb+qsec,mtcars)
fit7<-lm(mpg~am+wt+gear+disp+drat+carb+qsec+vs,mtcars)
fit8<-lm(mpg~.,mtcars)
fit9<-lm(mpg~am+wt+disp+carb,mtcars)
anova1<-anova(fit1, fit2,fit3,fit4,fit5, fit6, fit7,fit8,fit9)