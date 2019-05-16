
crime<-read.csv('C:\\Users\\Арнагуль\\Desktop\\RFiles\\crime.csv',header = TRUE)
a<-lm((crime)~(pctmetro) + (pctwhite) +(poverty) + (pcths) +(single), data=crime)
loga <-lm(data=crime, log(crime)~log(pctmetro)+ log(single)+ log(pctwhite) + poverty)
a$resi<-a$residuals

form<-(crime)~(pctmetro) + (pctwhite) +(poverty) + (single)
wi<-lm((crime)~(pctmetro) + (pctwhite) +(poverty) + (single), data=crime, model='within')

re<-lm((crime)~(pctmetro) + (pctwhite) +(poverty) + (single), data=crime, model='random')
phtest(wi, re)
summary(a)
par(mfrow=c(2,2)) 
plot(a)
plot(loga)
bptest(a) 
confint(a)
coeftest(a)



a<-lm((crime)~(pctmetro) + (pctwhite) +(poverty) + (single), data=crime)
a$resi <- a$residuals
varfunc<- lm((a$resi^2) ~ log(pctmetro)+ log(pctwhite) +log(poverty) + log(single), data = crime)
a$varfunc <- exp(varfunc$fitted.values)
a.gls <- lm(crime ~ pctmetro +pctwhite +(poverty) + (single), weights = 1/(sqrt(a$varfunc)), data = crime)