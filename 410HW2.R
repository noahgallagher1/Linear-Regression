########4.1#########
exercise1<-read.csv(file='/Users/noahgallagher1/Documents/FALL 2020/STAT 410/studymaterials/DataExercises/Exercise4.1Data.csv',
header=TRUE, sep=',')

#RESCALE
GMAT.res<- exercise1$GMAT/100

#RUNNING FITTED MODEL
#install.packages("ordinal")
library(ordinal)
summary(fitted.model<- clm(as.factor(status) ~ GPA + GMAT.res, data=exercise1, link="logit"))
#FINDING AIC
p<- 2
n<- 42
print(AICC<- -2*logLik(fitted.model)+2*p*n/(n-p-1))
#Getting BIC
BIC(fitted.model)
#Now we can check the goodness of fit
null.model<- clm(as.factor(status) ~ 1, data=exercise1, link="logit") 
#Computing and printing deviance
deviance<- -2*(logLik(null.model)-logLik(fitted.model))
print(deviance)
#computing pvalue
p.value<- pchisq(deviance, df=2, lower.tail = FALSE)
print(p.value)
#Computing Prediction
print(predict(fitted.model, type='prob', data.frame(GPA=3.1, GMAT.res=5.50)))

#################

#fitting cumulative probit model
admission.data<- read.csv(file='/Users/noahgallagher1/Documents/FALL 2020/STAT 410/studymaterials/DataExercises/Exercise4.1Data.csv', header= TRUE, sep=',')
#rescaling predictor
GMAT.res<- admission.data$GMAT/100
#running the model
library(ordinal)
summary(fitted.model<- clm(as.factor(status) ~ GPA + GMAT.res, data=admission.data, link="probit"))
#computing AICC
p<- 2
n<- 42
print(AICC<- -2*logLik(fitted.model)+2*p*n/(n-p-1))
#outputting BIC 
BIC(fitted.model)
#checking model fit
null.model<- clm(as.factor(status) ~ 1, data=admission.data, link="probit") 
print(deviance<- -2*(logLik(null.model)-logLik(fitted.model)))
print(p.value<- pchisq(deviance, df=2, lower.tail = FALSE))
#using fitted model for prediction
print(predict(fitted.model, type='prob', data.frame(GPA=3.1, GMAT.res=5.50)))

#################

exercise1<-read.csv(file='/Users/noahgallagher1/Documents/FALL 2020/STAT 410/studymaterials/DataExercises/Exercise4.1Data.csv',
                    header=TRUE, sep=',')

#RESCALE
GMAT.res<- exercise1$GMAT/100

#RUNNING FITTED MODEL
#install.packages("ordinal")
library(ordinal)
summary(fitted.model<- clm(as.factor(status) ~ GPA + GMAT.res, data=exercise1, link="cloglog"))
#FINDING AIC
p<- 2
n<- 42
print(AICC<- -2*logLik(fitted.model)+2*p*n/(n-p-1))
#Getting BIC
BIC(fitted.model)
#Now we can check the goodness of fit
null.model<- clm(as.factor(status) ~ 1, data=exercise1, link="cloglog") 
#Computing and printing deviance
deviance<- -2*(logLik(null.model)-logLik(fitted.model))
print(deviance)
#computing pvalue
p.value<- pchisq(deviance, df=2, lower.tail = FALSE)
print(p.value)
#Computing Prediction
print(predict(fitted.model, type='prob', data.frame(GPA=3.1, GMAT.res=5.50)))

########2#########
#fitting cumulative logit model
ex2.data<- read.csv(file='/Users/noahgallagher1/Documents/FALL 2020/STAT 410/studymaterials/DataExercises/Exercise4.2Data.csv', header= TRUE, sep=',')
#making response a categorical variable
satisf.cat<- as.factor(ex2.data$satisf)
#specifying reference categories
magazine.rel<- relevel(as.factor(ex2.data$magazine), ref="yes") 
resolved.rel<- relevel(as.factor(ex2.data$resolved), ref="yes")
#running the model
library(ordinal)
summary(fitted.model<- clm(satisf.cat ~ subscribed + magazine.rel + resolved.rel, data=ex2.data, link="cloglog"))
#computing AICC
p<-5
n<-36
print(AICC<- -2*logLik(fitted.model)+2*p*n/(n-p-1))
#outputting BIC 
BIC(fitted.model)
#checking model fit
null.model<- clm(satisf.cat ~ 1, data=ex2.data, link="cloglog") 
print(deviance<- -2*(logLik(null.model)-logLik(fitted.model)))

print(p.value<- pchisq(deviance, df=3, lower.tail = FALSE))
#using fitted model for prediction
print(predict(fitted.model, type='prob', data.frame(subscribed=3, magazine.rel='no', resolved.rel='yes')))

######4.4########
#read in data
ex.4.4<-read.csv(file='/Users/noahgallagher1/Documents/FALL 2020/STAT 410/studymaterials/DataExercises/Exercise4.4Data.csv',
                 header=TRUE,sep=',')
#references
outcome.rel<-relevel(as.factor(ex.4.4$outcome),ref='C')
water.rel<-relevel(as.factor(ex.4.4$water),ref='yes')

#fit generalized logit model
library(nnet)
summary(fitted.model<- multinom(outcome.rel ~ elevation + winddir + windspeed + water.rel, data=ex.4.4))

#goodness of fit
null.model<- multinom(outcome.rel ~ 1, data=ex.4.4) 
#deviance
deviance<- deviance(null.model)-deviance(fitted.model)
print(deviance)

#p-value
p.value<- pchisq(deviance, df=8, lower.tail = FALSE)
print(p.value)

#prediction
predict(fitted.model, type='prob', data.frame(elevation=2000, winddir=90, windspeed=5, water.rel='no'))

########4.5########
#reading in data
ex5<-read.csv(file='/Users/noahgallagher1/Documents/FALL 2020/STAT 410/studymaterials/DataExercises/Exercise4.5Data.csv',
              header=TRUE,sep=',')
#references
condition.rel<- relevel(as.factor(ex5$condition), ref="sprained") 
gender.rel<- relevel(as.factor(ex5$gender), ref="female")
#Fitting generalized logit
library(nnet)
summary(fitted.model<- multinom(condition.rel ~ age + gender.rel, data=ex5))
#goodness-of-fit (intercept only)
summary(null.model<- multinom(condition.rel ~ 1, data=ex5)) 
#deviance
deviance<- deviance(null.model)-deviance(fitted.model)
print(deviance)

#pvalue
p<- pchisq(deviance, df=4, lower.tail = FALSE)
print(p)

#Prediction
predict(fitted.model, type='prob', data.frame(age=9, gender.rel='female'))

#####5.1#####
#reading in data
ex5.data<- read.csv(file='/Users/noahgallagher1/Documents/FALL 2020/STAT 410/studymaterials/DataExercises/Exercise5.1Data.csv', 
                    header= TRUE, sep=',')
#releveling
shift.rel<- relevel(as.factor(ex5.data$shift), ref="morning")

#fitting Poisson Regression model
summary(fitted.model<- glm(ndefectives ~ experience + shift.rel, data=ex5.data, family=poisson(link=log)))

#goodness-of-fit
null.model<- glm(ndefectives ~ 1, data=ex5.data,family=poisson(link=log)) 
#Deviance
deviance<- -2*(logLik(null.model)-logLik(fitted.model))
print(deviance)
#p-value
p<- pchisq(deviance, df=4, lower.tail = FALSE)
print(p)

#Prediciton
predict(fitted.model, data.frame(experience=6, shift.rel='night'), type='response')

########5.2#######
insurance<- read.csv(file='/Users/noahgallagher1/Documents/FALL 2020/STAT 410/studymaterials/DataExercises/Exercise5.2Data.csv', 
                     header=TRUE, sep=',')
#specifying reference
gender.rel<- relevel(as.factor(insurance$gender), ref="F")

#Poisson Model
summary(fitted.model<- glm(naccidents ~ gender.rel + age + miles, data=insurance, family=poisson(link=log)))

#goodness-of-fit
null.model<- glm(naccidents ~ 1, data=insurance, family=poisson(link=log))
#Deviance
deviance<- -2*(logLik(null.model)-logLik(fitted.model))
print(deviance)

#p-value
p<- pchisq(deviance, df=3, lower.tail = FALSE)
print(p)

#Computing Prediction
predict(fitted.model, data.frame(gender.rel='F', age=35, miles=100), type='response')

####5.2#####
#reading in data
items.data<- read.csv(file='/Users/noahgallagher1/Documents/FALL 2020/STAT 410/studymaterials/DataExercises/Exercise5.1Data.csv', 
                      header= TRUE, sep=',')
#getting rid of zeros
items.data<- items.data[which(items.data$ndefectives != 0),]
#fitting zero-truncated Poisson model
#install.packages('VGAM')
library(VGAM)
summary(fitted.model<- vglm(ndefectives ~ experience + shift, data=items.data, family = pospoisson()))

#goodness-of-fit
null.model<- vglm(ndefectives ~ 1, data=items.data, family = pospoisson())
#Deviance
deviance<- -2*(logLik(null.model)-logLik(fitted.model))
print(deviance)

#p-value
p<- pchisq(deviance, df=4, lower.tail = FALSE)
print(p)

#Prediction
print(predict(fitted.model, data.frame(experience=6, shift='night'), type='response'))

####5.5####
#reading in the data
policy.data<-read.csv(file='/Users/noahgallagher1/Documents/FALL 2020/STAT 410/studymaterials/DataExercises/Exercise5.2Data.csv',
                      header=TRUE,sep=',')
#getting rid of zeros
policy.data<- policy.data[which(policy.data$naccidents != 0),]

#Zero-Truncated Poisson
library(VGAM)
summary(fitted.model<- vglm(naccidents ~ gender + age + miles, data=policy.data, family = pospoisson()))

#goodness-of-fit test
null.model<- vglm(naccidents ~ 1, data=policy.data, family = pospoisson()) 
#Deviance
deviance<- -2*(logLik(null.model)-logLik(fitted.model))
print(deviance)

#P-value
p<- pchisq(deviance, df=3, lower.tail = FALSE)
print(p)

#Prediction
predict(fitted.model, data.frame(gender='F', age=35, miles=100), type='response')

#######5.8#######
#reading in data
race<-read.csv(file='/Users/noahgallagher1/Documents/FALL 2020/STAT 410/studymaterials/DataExercises/Exercise5.8Data.csv',
               header = TRUE, sep=',')

#specifying references
run.rel<- relevel(as.factor(race$run), ref="5K") 
gender.rel<- relevel(as.factor(race$gender), ref="M")

#zero-inflated poisson
#install.packages('pscl')
library(pscl)
summary(fitted.model<- zeroinfl(nraces ~ gender.rel + age + run.rel | pace, data=race))

#goodness-of-fit test
null.model<- zeroinfl(nraces ~ 1, data=race) 

#deviance
deviance<- -2*(logLik(null.model)-logLik(fitted.model))
print(deviance)

p<- pchisq(deviance, df=5, lower.tail = FALSE)
print(p)

#Prediction
predict(fitted.model, data.frame(gender.rel='F', run.rel='10K', age=45, pace=10))

###########5.9##########
#reading in data
school.data<-read.csv(file='/Users/noahgallagher1/Documents/FALL 2020/STAT 410/studymaterials/DataExercises/Exercise5.9Data.csv',
                     header=TRUE,sep=',')
#Defining reference
HW.rel<- relevel(as.factor(school.data$hw), ref="no") 
GEN.rel<- relevel(as.factor(school.data$gender), ref="M")

#Zero-inflated poisson model
library(pscl)
summary(fitted.model<- zeroinfl(nbooks ~ HW.rel + GEN.rel | grade, data=school.data))

#goodness-of-fit
null.model<- zeroinfl(nbooks ~ 1, data=school.data) 
#deviance
deviance<- -2*(logLik(null.model)-logLik(fitted.model))
print(deviance)

#p-value
p<- pchisq(deviance, df=3, lower.tail = FALSE)
print(p)

#Prediction
predict(fitted.model, data.frame(grade=2, GEN.rel='F', HW.rel='yes'))

##################5.12#################
#reading in data
library.data<-read.csv(file='/Users/noahgallagher1/Documents/FALL 2020/STAT 410/studymaterials/DataExercises/Exercise5.12Data.csv',
                       header=TRUE,sep=',')
#Hurdle Poisson Regression Model 
library(pscl)
summary(fitted.model<- hurdle(ncomps ~ nbooks + njrnls | budget, data=library.data, dist='poisson', zero.dist='binomial', link='logit'))

#Goodness-of-fit
null.model<- hurdle(ncomps ~ 1, data=library.data, dist='poisson', zero.dist='binomial', link='logit')

#Deviance
deviance<- -2*(logLik(null.model)-logLik(fitted.model))
print(deviance)

#P-value
p<- pchisq(deviance, df=3, lower.tail = FALSE)
print(p)

#Prediction
predict(fitted.model, data.frame(nbooks=10, njrnls=25, budget=15))

####5.13####
#reading in data
health.data<-read.csv(file='/Users/noahgallagher1/Documents/FALL 2020/STAT 410/studymaterials/DataExercises/Exercise5.13Data.csv',
                      header=TRUE, sep=',')
#references
gender.rel<-relevel(as.factor(health.data$gender),ref="M")

#Hurdle Poisson
library(pscl)
summary(fitted.model<- hurdle(ndaysnomeds ~ gender.rel + age | nothermeds, data=health.data, dist='poisson', zero.dist = 'binomial', link='logit'))

#goodness of fit
null.model<- hurdle(ndaysnomeds ~ 1, data=health.data, dist='poisson', zero.dist='binomial', link='logit')
#Deviance
deviance<- -2*(logLik(null.model)-logLik(fitted.model))
print(deviance)

#p-value
print(p<- pchisq(deviance, df=3, lower.tail = FALSE))

#Prediction
predict(fitted.model, data.frame(gender.rel='M', age=78, nothermeds=0))

####6.2#####
#reading in data
mussels.data<- read.csv(file='/Users/noahgallagher1/Documents/FALL 2020/STAT 410/studymaterials/DataExercises/Exercise6.2Data.csv', header=TRUE, sep=',')
#Reference
feedinglevel.rel<- relevel(as.factor(mussels.data$feeding.level), ref="high")
#Negative Binomial Model
library(MASS)
summary(fitted.model<- glm.nb(ndead.mussels ~ max.temp + min.temp + feedinglevel.rel, data=mussels.data))

#goodness of fit
null.model<- glm.nb(ndead.mussels ~ 1, data = mussels.data) 

#deviance
print(dev<- -2*(logLik(null.model)-logLik(fitted.model)))

#p-val
print(p<- pchisq(deviance, df=4, lower.tail = FALSE))

#Prediction
predict(fitted.model, data.frame(max.temp=75, min.temp=60, feedinglevel.rel='high'), type='response')

######6.3######
#reading in data
allowance.data<- read.csv(file='/Users/noahgallagher1/Documents/FALL 2020/STAT 410/studymaterials/DataExercises/Exercise6.3Data.csv', header=TRUE, sep=',')
#References
gender.rel<- relevel(as.factor(allowance.data$gender), ref="M") 
job.rel<- relevel(as.factor(allowance.data$job), ref="yes")
#Negative Binomial Model
library(MASS)
summary(fitted.model<- glm.nb(allowance ~ age + gender.rel + job.rel, data=allowance.data))
#goodness of fit
null.model<- glm.nb(allowance ~ 1, data=allowance.data)
#deviance
print(dev<- -2*(logLik(null.model)-logLik(fitted.model)))
#p-val
print(p<- pchisq(deviance, df=3, lower.tail = FALSE))

#Prediction
predict(fitted.model, data.frame(age=16, gender.rel='M', job.rel='no'), type='response')

####6.4#####
#read in data
park.data<- read.csv(file='/Users/noahgallagher1/Documents/FALL 2020/STAT 410/studymaterials/DataExercises/Exercise6.4Data.csv', header = TRUE, sep=',')
#reference
camped.rel<- relevel(as.factor(park.data$camped), ref="yes")
#Zero-truncated negative binomial model
library(VGAM)
summary(fitted.model<- vglm(nkayaks ~ partysize + routelength + camped.rel, data=park.data, family=posnegbinomial()))
#goodness of fit
null.model<- vglm(nkayaks ~ 1, data=park.data, family=posnegbinomial()) 
#deviance
print(dev<- -2*(logLik(null.model)-logLik(fitted.model)))
#p-val
print(p<- pchisq(deviance, df=3, lower.tail = FALSE))
#Prediction
predict(fitted.model, data.frame(partysize=5,routelength=6, camped.rel='yes'), type='response')

####6.5####
#read in data
videos.data<- read.csv(file='/Users/noahgallagher1/Documents/FALL 2020/STAT 410/studymaterials/DataExercises/Exercise6.5Data.csv', header=TRUE, sep=',')
#reference
type.rel<- relevel(as.factor(videos.data$type), ref="fashion")
#Zero-truncated negative binomial model
library(VGAM)
summary(fitted.model<- vglm(nnewvideos ~ nvideos + nsubscr + nviews + type.rel, data=videos.data, family = posnegbinomial()))
#goodness of fit
null.model<- vglm(nnewvideos ~ 1, data = videos.data, family=posnegbinomial()) 
#deviance
print(dev<- -2*(logLik(null.model)-logLik(fitted.model)))
#p-val
print(p<- pchisq(deviance, df=8, lower.tail = FALSE))
#Prediction
print(predict(fitted.model, data.frame(nvideos=87, nsubscr=50, nviews=254, type.rel='science'), type='response'))

#########6.6#######
#reading in data
insurance.data<- read.csv(file='/Users/noahgallagher1/Documents/FALL 2020/STAT 410/studymaterials/DataExercises/Exercise6.6Data.csv', header=TRUE, sep=',')
#Reference
gender.rel<- relevel(as.factor(insurance.data$gender), ref="M")
#Zero-inflated negative binomial model
library(pscl)
summary(fitted.model<- zeroinfl(nclaimspast5ys ~ age + gender.rel | nclaimsprev5ys, data=insurance.data, dist='negbin'))
#goodness of fit
null.model<- zeroinfl(nclaimspast5ys ~ 1, data=insurance.data, dist='negbin')
#deviance
print(dev<- -2*(logLik(null.model)-logLik(fitted.model)))
#p-val
print(p<- pchisq(deviance, df=3, lower.tail = FALSE))
#Prediction
predict(fitted.model, data.frame(nclaimsprev5ys=0, age=55, gender.rel='F'))

###6.7#####
dental.data<- read.csv(file='/Users/noahgallagher1/Documents/FALL 2020/STAT 410/studymaterials/DataExercises/Exercise6.7Data.csv', header = TRUE, sep=',')

#Reference 
gender.rel<- relevel(as.factor(dental.data$gender), ref="F") 
oralhygiene.rel<- relevel(as.factor(dental.data$oralhygiene), ref="med")

#zero-inflated negative binomial model
library(pscl)
summary(fitted.model<- zeroinfl(DMFTindex ~ gender.rel + oralhygiene.rel | age, data=dental.data, dist='negbin'))

#goodness of fit
null.model<- zeroinfl(DMFTindex ~ 1, data=dental.data, dist='negbin') 
#deviance
print(dev<- -2*(logLik(null.model)-logLik(fitted.model)))

#p-val
print(p<- pchisq(deviance, df=4, lower.tail = FALSE))

#Prediction
predict(fitted.model, data.frame(age=28, gender.rel='M', oralhygiene.rel='high'))

####6.8#####
#reading in data
insur.data<- read.csv(file='/Users/noahgallagher1/Documents/FALL 2020/STAT 410/studymaterials/DataExercises/Exercise6.6Data.csv', header=TRUE, sep=',')
#reference
gender.rel<- relevel(as.factor(insur.data$gender), ref="M")
#hurdle negative binomial model
library(pscl)
summary(fitted.model<- hurdle(nclaimspast5ys ~ age + gender.rel | nclaimsprev5ys, data=insur.data, dist='negbin', zero.dist = 'binomial', link='logit'))
#goodnesss-of-fit
null.model<- hurdle(nclaimspast5ys ~ 1, data=insur.data, dist='negbin', zero.dist='binomial', link='logit')
#deviance
print(dev<- -2*(logLik(null.model)-logLik(fitted.model)))
#p-val
print(p<- pchisq(deviance, df=3, lower.tail = FALSE))
#Prediction
predict(fitted.model, data.frame(nclaimsprev5ys=0, age=55, gender.rel='F'))

#####6.9######
#reading in data
sports.data<- read.csv(file='/Users/noahgallagher1/Documents/FALL 2020/STAT 410/studymaterials/DataExercises/Exercise6.9Data.csv', header=TRUE, sep=',')
#reference
gender.rel<- relevel(as.factor(sports.data$gender), ref="F")
#hurdle-negative-binomial model
library(pscl)
summary(fitted.model<- hurdle(ngameinjuries ~ gender.rel + nsports|npracticeinjuries, data=sports.data, dist='negbin', zero.dist='binomial', link='logit'))
#goodness of fit
null.model<- hurdle(ngameinjuries ~ 1, data=sports.data, dist='negbin', zero.dist = 'binomial', link='logit')
#deviance
print(dev<- -2*(logLik(null.model)-logLik(fitted.model)))
#p-val
print(p<- pchisq(deviance, df=3, lower.tail = FALSE))
#Prediction
predict(fitted.model, data.frame(gender.rel='M', nsports=2, npracticeinjuries=1))
#####7.2#####
#reading in data
birds.data<- read.csv(file='/Users/noahgallagher1/Documents/FALL 2020/STAT 410/studymaterials/DataExercises/Exercise7.2Data.csv', header=TRUE, sep=',')
#rescaling to match problem
mass<- birds.data$mass/1000
wings<- birds.data$wingspan/100
dist<- birds.data$distance/1000
#proportion to model
propsuccess<- birds.data$nmigrated/birds.data$nringed
#beta model
#install.packages("betareg")
library(betareg)
summary(fitted.model<- betareg(propsuccess ~ mass + wings + dist, link='logit'))
#goodness of fit
null.model<- betareg(propsuccess ~ 1, link='logit') 
#deviance
print(dev<- -2*(logLik(null.model)-logLik(fitted.model)))
#p-val
print(p<- pchisq(deviance, df=3, lower.tail = FALSE))
#prediction
70*predict(fitted.model, data.frame(mass=.6, wings=.65, dist=1.65, nringed=70))

#######7.3########
#read in data
hosp.data<- read.csv(file='/Users/noahgallagher1/Documents/FALL 2020/STAT 410/studymaterials/DataExercises/Exercise7.3Data.csv', header=TRUE, sep=',')
#reference levels 
location.rel<- relevel(as.factor(hosp.data$location), ref="urban") 
type.rel<- relevel(as.factor(hosp.data$type), ref="private")
#proportion
prophospitalized<- hosp.data$perchospitalized/100 
#beta regression model
library(betareg)
summary(fitted.model<- betareg(prophospitalized ~ location.rel + type.rel + nbeds, data=hosp.data, link='logit'))
#goodness of fit
null.model<- betareg(prophospitalized ~ 1, data=hosp.data, link='logit') 
#deviance
print(dev<- -2*(logLik(null.model)-logLik(fitted.model)))
#p-val
print(p<- pchisq(deviance, df=3, lower.tail = FALSE))
#using fitted model for prediction
predict(fitted.model, data.frame(location.rel='rural', type.rel='public', nbeds=50))

####7.5#####
#readingin data
realestate.data<- read.csv(file='/Users/noahgallagher1/Documents/FALL 2020/STAT 410/studymaterials/DataExercises/Exercise7.5Data.csv', header= TRUE, sep=',')
#rescaling predictors 
realestate.data$propsold<- realestate.data$percsold/100 
realestate.data$avgprice.res<- realestate.data$avgprice/100 
realestate.data$nhouses.res<- realestate.data$nhouses/100
#zero-inflated beta regression model
#install.packages("gamlss")
library(gamlss)
summary(fitted.model<- gamlss(propsold ~ avgprice.res + nhouses.res, mu.link='logit', nu.formula = ~ age, nu.link='logit', data=realestate.data, family = BEZI))
#goodness of fit
null.model<- gamlss(propsold ~ 1, mu.link='logit', nu.formula= ~ 1, nu.link='logit', data=realestate.data, family=BEZI) 
#deviance
print(dev<- -2*(logLik(null.model)-logLik(fitted.model)))
#p-val
print(p<- pchisq(deviance, df=3, lower.tail = FALSE))
#using fitted model for prediction
pred<- predictAll(fitted.model, newdata=data.frame(avgprice.res=4.5, nhouses.res=3, age=50), type='response') 
print((1-pred$nu)*pred$mu)
########7.6########
studio.data<- read.csv(file='/Users/noahgallagher1/Documents/FALL 2020/STAT 410/studymaterials/DataExercises/Exercise7.6Data.csv', header=TRUE, sep=',')
#computing the response variable 
studio.data$propfirst<-studio.data$nfirstplaces/studio.data$ntrophies
#fitting zero-inflated beta regression model
library(gamlss)
summary(fitted.model<- gamlss(propfirst ~ nyears + nblackbelts, mu.link='logit', nu.formula = ~ npupils, nu.link='logit', data=studio.data, family=BEZI))
#goodness of fit
null.model<- gamlss(propfirst ~ 1, mu.link='logit', nu.formula = ~ 1, nu.link='logit', data=studio.data, family=BEZI)
#deviance
print(dev<- -2*(logLik(null.model)-logLik(fitted.model)))
#p-val
print(p<- pchisq(deviance, df=3, lower.tail = FALSE))
#computing prediction
param.pred<- predictAll(fitted.model, newdata=data.frame(nyears=10, nblackbelts=3, npupils=85), type='response') 
#printing
print((1-param.pred$nu)*param.pred$mu)

#####7.7#####
#read in data
trees.data<- read.csv(file='/Users/noahgallagher1/Documents/FALL 2020/STAT 410/studymaterials/DataExercises/Exercise7.7Data.csv', header=TRUE, sep=',')
#response variable
trees.data$propsurvived<- trees.data$nsurvived/trees.data$nplanted
#one-inflated beta model
library(gamlss)
summary(fitted.model<- gamlss(propsurvived ~ pestcontrol + fertilization, mu.link='logit', nu.formula = ~ precipitation + windspeed, nu.link='logit', data=trees.data, family=BEOI))
#goodness-of-fit
null.model<- gamlss(propsurvived ~ 1, mu.link='logit', nu.formula=~1, nu.link='logit', data=trees.data, family=BEOI)
#deviance
print(dev<- -2*(logLik(null.model)-logLik(fitted.model)))
#p-val
print(p<- pchisq(deviance, df=4, lower.tail = FALSE))
#Computing prediction
param1<- predictAll(fitted.model, newdata = data.frame(pestcontrol=0, fertilization=0, precipitation=2, windspeed=12.5), type='response') 
param2<- predictAll(fitted.model, newdata = data.frame(pestcontrol=0, fertilization=0, precipitation=25, windspeed=6), type='response') 
#printing param 1 & 2 after eqn
print(param1$nu+(1-param1$nu)*param1$mu) 
print(param2$nu+(1-param2$nu)*param2$mu)

######7.8#######
#read in data
s.data<- read.csv(file='/Users/noahgallagher1/Documents/FALL 2020/STAT 410/studymaterials/DataExercises/Exercise7.8Data.csv', header=TRUE, sep=',')
#references 
s.data$gender.rel<- relevel(as.factor(s.data$gender), ref="M")
#modeling one-inflated beta regression
library(gamlss)
summary(fitted.model<- gamlss(propsales ~ gender.rel + bonus, mu.link='logit', nu.formula = ~ expyr, nu.link='logit', data=s.data, family=BEOI))
#goodness-of-fit
null.model<- gamlss(propsales ~ 1, mu.link='logit', nu.formula = ~ 1, nu.link='logit', data=s.data, family=BEOI)
#deviance
print(dev<- -2*(logLik(null.model)-logLik(fitted.model)))
#p-val
print(p<- pchisq(deviance, df=3, lower.tail = FALSE))
#computing prediction
param<- predictAll(fitted.model, newdata = data.frame(expyr=3, gender.rel='M', bonus=1.5), type='response')
#printing *
print(param$nu+(1-param$nu)*param$mu)

#######7.9#########
#reading in data
corn.data<- read.csv(file='/Users/noahgallagher1/Documents/FALL 2020/STAT 410/studymaterials/DataExercises/Exercise7.9Data.csv', header=TRUE, sep=',')
#re-scaling data
corn.data$altitudeK<- corn.data$altitude/1000
#modeling zero-one-inflated beta regression
library(gamlss)
summary(fitted.model<- gamlss(germrate ~ altitudeK, mu.link='logit', nu.formula = ~ EC, nu.link='log', tau.formula = ~ soiltemp, tau.link='log', data=corn.data, family=BEINF))
#goodness-of-fit
null.model<- gamlss(germrate ~ 1, mu.link='logit', nu.formula = ~ 1, nu.link='log', tau.formula = ~ 1, tau.link='log', data=corn.data, family=BEINF) 
#deviance
print(deviance<- -2*(logLik(null.model)-logLik(fitted.model)))
#p-val
print(p.value<- pchisq(deviance, df=3, lower.tail = FALSE))
#computing prediction
param<- predictAll(fitted.model, newdata = data.frame(EC=1.5, soiltemp=68, altitudeK=0.95), type='response') 
#printing predicted param
print((param$tau+param$mu)/(1+param$nu+param$tau))

#####7.10#######
#read in data
coll.foot.data<- read.csv(file='/Users/noahgallagher1/Documents/FALL 2020/STAT 410/studymaterials/DataExercises/Exercise7.10Data.csv', header=TRUE, sep=',')
#modeling zero-one-inflated beta regression
library(gamlss)
summary(fitted.model<- gamlss(propgames ~ vertical + bench, mu.link='logit', nu.formula = ~ broad, nu.link='log',tau.formula = ~ BMI + fortyyd, tau.link ='log', data=coll.foot.data, family=BEINF))
#goodness-of-fit
null.model<- gamlss(propgames ~ 1, mu.link='logit', nu.formula = ~ 1, nu.link='log', tau.formula = ~ 1, tau.link='log', data=coll.foot.data, family= BEINF)
#deviance
print(dev<- -2*(logLik(null.model)-logLik(fitted.model)))
#p-val
print(p<- pchisq(deviance, df=5, lower.tail = FALSE))
#computing prediction
param<- predictAll(fitted.model, newdata = data.frame(BMI=27.8, fortyyd=4.67, vertical=32, broad=117, bench=16), type='response') 
#printing param & estimating
print((param$tau+param$mu)/(1+param$nu+param$tau))
