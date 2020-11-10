############Q1###########
drug_eff.data<- read.csv(file="/Users/noahgallagher1/Downloads/study_materials/DataExercises/Exercise1.2Data.csv",
                                 header=TRUE, sep=",")
#plotting histogram with fitted normal density
library(rcompanion)
plotNormalHistogram(drug_eff.data$EWL)

#testing normality of distribution 
shapiro.test(drug_eff.data$EWL)

#specifying reference levels
drug.rel<- relevel(as.factor(drug_eff.data$drug), ref="A") 
gender.rel<- relevel(as.factor(drug_eff.data$gender), ref="M")

#fitting general linear model
summary(fitted.model<- glm(EWL ~ drug.rel + age + gender.rel, 
                           data=drug_eff.data, family=gaussian(link=identity)))

#outputting estimated sigma
sigma(fitted.model)

#checking model fit 
null.model<- glm(EWL ~ 1, data=drug_eff.data, 
                 family=gaussian(link=identity))
print(deviance<- -2*(logLik(null.model)-logLik(fitted.model)))
print(p.value<- pchisq(deviance, df=4, lower.tail=FALSE))

#using fitted model for prediction
print(predict(fitted.model, data.frame(drug.rel="A", 
                                       age=35, gender.rel="M")))
#########Q2#########
car_info.data<- read.csv(file="/Users/noahgallagher1/Documents/FALL 2020/STAT 410/studymaterials/DataExercises/Exercise1.3Data.csv",
                         header=TRUE, sep=",")
#create price a factor of 1000
fact_price<-car_info.data$price/1000
#plotting histogram with fitted normal density
library(rcompanion)
plotNormalHistogram(fact_price)
#testing normality
shapiro.test(fact_price)

#specifying ref levels
style.rel<-relevel(as.factor(car_info.data$bodystyle),ref="coupe")
country.rel<-relevel(as.factor(car_info.data$country),ref="usa")
doors.rel<-relevel(as.factor(car_info.data$doors),ref="4")
leather.rel<-relevel(as.factor(car_info.data$leather),ref="yes")

#fitting gen linear model
summary(fitted.model<-glm(fact_price ~ style.rel+country.rel+hwy+doors.rel+leather.rel,
                          data=car_info.data,family=gaussian(link=identity)))

#outputting sigma(est)
sigma(fitted.model)

#checking model fit
null.model<-glm(fact_price ~ 1,data=car_info.data,
                family=gaussian(link=identity))
print(deviance<- -2*(logLik(null.model)-logLik(fitted.model)))
print(p.value<-pchisq(deviance,df=7,lower.tail=FALSE)) #df=7 for 7 variables

#using fitted model for prediction
print(predict(fitted.model,data.frame(style.rel="coupe",country.rel="usa",doors.rel="4",leather.rel="yes",hwy=30))*1000)

###########Q3############
child_obestiy.data<-read.csv(file = "/Users/noahgallagher1/Documents/FALL 2020/STAT 410/studymaterials/DataExercises/Exercise2.1Data.csv",
                             header=TRUE, sep=",")
#rescale var's & specify reference
BMI_perc<- child_obestiy.data$preBMI - child_obestiy.data$postBMI
gender.rel<- relevel(as.factor(child_obestiy.data$gender), ref="F")
group.rel<- relevel(as.factor(child_obestiy.data$group), ref="Cx")

#plot histogram
library(rcompanion)
plotNormalHistogram(BMI_perc)

#testing normality
shapiro.test(BMI_perc)

#finding optimal lambda
library(MASS)
BoxCox.fit<- boxcox(BMI_perc ~ age + gender.rel + group.rel, data=child_obestiy.data, lambda=seq(-3,3,1/4), interp=FALSE)
BoxCox.data<- data.frame(BoxCox.fit$x, BoxCox.fit$y)
ordered.data<- BoxCox.data[with(BoxCox.data, order(-BoxCox.fit.y)),]
ordered.data[1,]

#applying box cox trans with lambda=0
tr.BMI_perc<- log(BMI_perc)

#plotting histogram for transformed response
plotNormalHistogram(tr.BMI_perc)

#testing normality of distn
shapiro.test(tr.BMI_perc)

#fitting general linear model
summary(fitted.model<-glm(tr.BMI_perc ~ age + gender.rel + group.rel,data=child_obestiy.data,family=gaussian(link=identity)))
print(sigma(fitted.model))

#check for fit
null.model<- glm(tr.BMI_perc ~ 1,family = gaussian(link=identity))
print(deviance<- -2*(logLik(null.model)-logLik(fitted.model)))
print(p.value<- pchisq(deviance,3,lower.tail = FALSE))

#use fitted model to predict
pred.tr.BMI<- predict(fitted.model,data.frame(age=9,gender.rel='F',group.rel='Cx'))
print(exp(pred.tr.BMI))

##########Q4##########
quality_study.data<-read.csv(file = "/Users/noahgallagher1/Documents/FALL 2020/STAT 410/studymaterials/DataExercises/Exercise2.2Data.csv",
                             header=TRUE, sep=",")
#rescale var's & specify reference
score_new<- quality_study.data$score/100
design.rel<- relevel(as.factor(quality_study.data$desgn),ref='doctor')
QI.rel<- relevel(as.factor(quality_study.data$priorQI),ref='yes')

#plot histogram
library(rcompanion)
plotNormalHistogram(score_new)

#testing normality
shapiro.test(score_new)

#finding optimal lambda
library(MASS)
BoxCox.fit<- boxcox(score_new ~ design.rel + wrkyrs + QI.rel, data=quality_study.data, lambda=seq(-3,3,1/4), interp=FALSE)
BoxCox.data<- data.frame(BoxCox.fit$x, BoxCox.fit$y)
ordered.data<- BoxCox.data[with(BoxCox.data, order(-BoxCox.fit.y)),]
ordered.data[1,]

#applying box cox trans with lambda=-1
tr_score<- 1-(1/score_new)

#plotting histogram for transformed response
plotNormalHistogram(tr_score)

#testing normality of distn
shapiro.test(tr_score)

#fitting general linear model
summary(fitted.model<-glm(tr_score ~ design.rel + wrkyrs + QI.rel,data=quality_study.data,family=gaussian(link=identity)))
print(sigma(fitted.model))

#check for fit
null.model<- glm(tr_score ~ 1,family = gaussian(link=identity))
print(deviance<- -2*(logLik(null.model)-logLik(fitted.model)))
print(p.value<- pchisq(deviance,4,lower.tail = FALSE))

#use fitted model to predict
pred.tr.score<- predict(fitted.model,data.frame(design.rel='nurse',wrkyrs=7,QI.rel='yes'))
print(100*(1/(1-pred.tr.score)))

##########Q5#########
child.obesity.data<- read.csv(file="/Users/noahgallagher1/Documents/FALL 2020/STAT 410/studymaterials/DataExercises/Exercise2.1Data.csv",
                              header=TRUE, sep=",")

#specify reference
gender.rel<-relevel(as.factor(child.obesity.data$gender),ref='M')
group.rel<-relevel(as.factor(child.obesity.data$group),ref='Cx')
new.bmi<-child.obesity.data$preBMI-child.obesity.data$postBMI

#fitting gamma regression
summary(fitted.model<-glm(new.bmi ~ gender.rel + age + group.rel, data=child.obesity.data, family=Gamma(link=log)))

#now we check model fit
null.model<-glm(new.bmi~1, family = Gamma(link=log))
print(deviance<- -2*(logLik(null.model)-logLik(fitted.model)))
print(p.value<- pchisq(deviance, df=3, lower.tail=FALSE))

#prediction
print(predict(fitted.model,type="response", data.frame(gender.rel='F',age=9,group.rel='Cx')))

#########Q6##########
qi.score.data<-read.csv(file="/Users/noahgallagher1/Documents/FALL 2020/STAT 410/studymaterials/DataExercises/Exercise2.2Data.csv",
                         header=TRUE, sep=",")

#relevel & reference
new.score<- qi.score.data$score/100
design.rel<-relevel(as.factor(qi.score.data$desgn),ref="doctor")
qi.rel<-relevel(as.factor(qi.score.data$priorQI),ref="yes")

#fitting gamma regression
summary(fitted.model<-glm(new.score~ design.rel + wrkyrs + qi.rel, data = qi.score.data,family=Gamma(link=log)))

#now we check model fit
null.model<-glm(new.score ~ 1, family=Gamma(link=log))
print(deviance<- -2*(logLik(null.model)-logLik(fitted.model)))
print(p.value<- pchisq(deviance, df=4,lower.tail=FALSE))

#prediction
print(predict(fitted.model,type="response",data.frame(design.rel='nurse',wrkyrs=7,qi.rel='yes'))*100)

############Q7############
#fitting complementary log log model
skin.data<-read.csv(file="/Users/noahgallagher1/Documents/FALL 2020/STAT 410/studymaterials/DataExercises/Exercise3.2Data.csv",
                    header=TRUE,sep=",")
#specifying reference
gender.rel<-relevel(as.factor(skin.data$gender),ref="M")
medication.rel<-relevel(as.factor(skin.data$medication),ref="A")

#running model
summary(fitted.model<-glm(response~gender.rel + age + medication.rel,data=skin.data,family=binomial(link=cloglog)))

#get AIC
p<-4
n<-30
AICC<- -2*logLik(fitted.model)+2*p*n/(n-p-1)
print(AICC)

#get BIC
BIC(fitted.model)

#check goodness of fit
null.model<-glm(response~1, data=skin.data, family=binomial(link=cloglog))
print(deviance<- -2*(logLik(null.model)-logLik(fitted.model)))
p<-pchisq(deviance,4,lower.tail=FALSE)
print(p)

#check prediction
print(predict(fitted.model,type='response',data.frame(gender.rel="F",age=50,medication.rel="A")))

#####Q8#########
#fitting complementary log log model
bank.data<-read.csv(file="/Users/noahgallagher1/Documents/FALL 2020/STAT 410/studymaterials/DataExercises/Exercise3.4Data.csv",
                    header=TRUE,sep=",")
#specifying reference
income.rel<-relevel(as.factor(bank.data$income),ref="high")
default.rel<-relevel(as.factor(bank.data$default),ref="No")

#running model
summary(fitted.model<-glm(default.rel~LTV+age+income.rel,data=bank.data,family=binomial(link=cloglog)))

#get AICC
p<-3
n<-35
AICC<- -2*logLik(fitted.model)+2*p*n/(n-p-1)
print(AICC)

#get BIC
BIC(fitted.model)

#check goodness of fit
null.model<-glm(default.rel~1, data=bank.data, family=binomial(link=cloglog))
print(deviance<- -2*(logLik(null.model)-logLik(fitted.model)))
p<-pchisq(deviance,3,lower.tail=FALSE)
print(p)

#check prediction
print(predict(fitted.model,type='response',data.frame(LTV=50,age=50,income.rel="high")))


