#######################Packages and data#######################
library("sandwich")
library("zoo")
library("lmtest")
library("MASS")
library("pscl")
library("LogisticDx")
library("ucminf")
library("ordinal")
library("reshape")
library("generalhoslem")
library("oglmx")
library("aod")
library("brant")
library("stargazer")
library("openxlsx")
library("dplyr")
library("effects")
library("VIF")
library("pryr")


# loading data
setwd("/Users/bartek")
dane <- read.xlsx("MarijuanaData.xlsx")
dane2 <- read.xlsx("MarijuanaData.xlsx")
# deleting unnecessary columns
dane <- select(dane, -stateAb)
dane <- select(dane, -binLegStat)
# selecting regressors
dane.var <- dane[, c(3:12)]
# designation variable as ordered
dane$legStat <- factor(dane$legStat, levels = c("0", "1", "2"), ordered = TRUE)
#dane$govern <- factor(dane$govern, levels = c("0", "1"), ordered = TRUE)

########################Exploratory data analysis #######################

# Summarizing the data
summary(dane)

# Making frequency table
table(dane$legStat, dane$govern)

# Rest of EDA is in Python's Jupyter Notebook

#######################Ordered logit#######################
# Estimate ordered logit

# polr from MASS package
dane2$legStat <- as.factor(dane2$legStat)
ologit <- polr(legStat~priOfJoi+odM+cannUse+opoiPres+tedsAM+tedsAP100k+arriv+vc100k+pc100k+govern, data=dane2, Hess=TRUE)
summary(ologit)
coeftest(ologit)
dane2$legStat <- as.factor(dane2$legStat)
# log(tedsAP100k) daje ** istotność priOfJoi
# log(priOfJoi) daje *** istotność priOfJoi
# inne wyniki, jak nie traktuje govern i legStat jako factor, tylko as.factor(legStat) pojawia się istotnosc govern, przy log(priOfJoi) i log(tedsAP100k)
# bez log, govern istotne na ***


# get the p-values
# store the coefficient table
ctable <- round(coef(summary(ologit)), 4)
# calculate and store p-values
p <- pnorm(abs(ctable[, "t value"]), lower.tail = F) * 2
# combine coefficient table and p-values table
(ctable <- cbind(ctable, "p value" = round(p, 4)))

# get confidence intervals
# profiled CIs
ci <- round(confint(ologit), 4)
# log odd coefficients
or <- round(coef(ologit), 4)
# convert coefficients into odds ratio, combine with CIs
round(exp(cbind(OR = or, ci)), 4)


# Are vars jointly significant?
# Likelihood ratio test
ologit.restricted = polr(as.factor(legStat)~1, data=dane)
lrtest(ologit, ologit.restricted)

#Compute confusion table and misclassification error
legStatPredict = predict(ologit,dane)
table(dane$legStat, legStatPredict)
mean(as.character(dane$legStat) != as.character(legStatPredict))

#Plotting the effects 
Effect(focal.predictors = "priOfJoi",ologit)
plot(Effect(focal.predictors = "priOfJoi",ologit))
plot(Effect(focal.predictors = c("cannUse", "priOfJoi"),ologit))

# marginal effects
options(scipen=999)
margins.oglmx(ologit)

# standarizing
dane_standardize <- as.data.frame(scale(dane[3:11]))
ologit2 <- polr(dane$legStat~priOfJoi+odM+cannUse+opoiPres+tedsAM+tedsAP100k+arriv+vc100k+pc100k, data=dane_standardize, Hess=TRUE)
summary(ologit2)
coeftest(ologit2)

# linktest
linktest(ologit)

# Assumptions

# goodness-of-fit tests
logitgof(dane$legStat, fitted(ologit), g = 10, ord = TRUE)
pulkrob.chisq(ologit, c("govern"))
lipsitz.test(ologit)
pulkrob.deviance(ologit, c("govern"))

# Goodness-of-fit tests
lipsitz.test(ologit)
logitgof(dane2$legStat, fitted(ologit), g = 5, ord = TRUE)

# Brant's test
# the function works with polr model results
brant(ologit)

# Pseudo-R2 statistics
pR2(ologitC)

# Multi-collinearity
# correlation plot
# dane.var <- dane[, c(3:12)]
ggpairs(dane.var, title = "Correlation Plot between each Variable")

# check VIF
fit2 <- lm(legStat~priOfJoi+odM+cannUse+opoiPres+tedsAM+tedsAP100k+arriv+vc100k+pc100k+govern, data=dane)
vif(fit2)

# proportional odds
# testing parallel regression assumption using Brant's test
brant(ologit)


# PCA
head(dane.var,5)
dane.var$govern <- as.factor(dane.var$govern)
dane.pca <- prcomp(dane.var[1:10], center=TRUE, scale.=TRUE)
print(dane.pca)
summary(dane.pca)

#######################Ordered probit#######################

#Logit and probit comparison 

reg <- lm(legStat~priOfJoi+odM+cannUse+opoiPres+tedsAM+tedsAP100k+arriv+vc100k+pc100k+govern, data=dane2)
summary(reg)

ologit <- polr(legStat~priOfJoi+odM+cannUse+opoiPres+tedsAM+tedsAP100k+arriv+vc100k+pc100k+govern, data=dane, Hess=TRUE)

oprobit <- polr(legStat~priOfJoi+odM+cannUse+opoiPres+tedsAM+tedsAP100k+arriv+vc100k+pc100k+govern, data=dane, Hess=TRUE, method = "probit")

stargazer(reg, ologit, oprobit, type="text", title="Models Predicting legStat",add.lines=list(c("BIC", round(BIC(reg),1), round(BIC(ologit),1), round(BIC(oprobit),1)),c("AIC", round(AIC(reg),1), round(AIC(ologit),1), round(AIC(oprobit),1))))

#######################Unspecified logits#######################

# dane2 dataset

ologit1 <- polr(as.factor(legStat)~priOfJoi+odM+cannUse+opoiPres+tedsAM+tedsAP100k+arriv+vc100k+pc100k+govern, data=dane2, Hess=TRUE)
#coeftest(ologit1)
ologit2 <- polr(as.factor(legStat)~log(priOfJoi)+odM+cannUse+opoiPres+tedsAM+log(tedsAP100k)+arriv+vc100k+pc100k+govern, data=dane2, Hess=TRUE)
#coeftest(ologit2)
ologit3 <- polr(as.factor(legStat)~priOfJoi+log(odM)+cannUse+opoiPres+tedsAM+tedsAP100k+arriv+vc100k+pc100k+govern, data=dane2, Hess=TRUE)
#coeftest(ologit3)
ologit4 <- polr(as.factor(legStat)~priOfJoi+odM+cannUse+log(opoiPres)+tedsAM+tedsAP100k+arriv+vc100k+pc100k+govern, data=dane2, Hess=TRUE)
#coeftest(ologit4)
ologit5 <- polr(as.factor(legStat)~priOfJoi+odM+cannUse+opoiPres+tedsAM+tedsAP100k+arriv+log(vc100k)+log(pc100k)+govern, data=dane2, Hess=TRUE)
#coeftest(ologit5)
ologit6 <- polr(as.factor(legStat)~priOfJoi+odM+cannUse+opoiPres+tedsAM+tedsAP100k+log(arriv)+vc100k+pc100k+govern, data=dane2, Hess=TRUE)
#coeftest(ologit6)
ologit7 <- polr(as.factor(legStat)~log(priOfJoi)+log(odM)+cannUse+log(opoiPres)+tedsAM+log(tedsAP100k)+log(arriv)+log(vc100k)+log(pc100k)+govern, data=dane2, Hess=TRUE)
#coeftest(ologit7)
ologit8 <- polr(as.factor(legStat)~priOfJoi+log(odM)+cannUse+log(opoiPres)+tedsAM+log(tedsAP100k)+log(arriv)+log(vc100k)+pc100k+govern, data=dane2, Hess=TRUE)
#coeftest(ologit8)

stargazer(ologit1, ologit2, ologit3, ologit4, ologit5, ologit6, ologit7, ologit8, type="text", title="Models Predicting legStat",add.lines=list(c("BIC", round(BIC(ologit1),1), round(BIC(ologit2),1), round(BIC(ologit3),1), round(BIC(ologit4),1), round(BIC(ologit5),1), round(BIC(ologit6),1), round(BIC(ologit7),1), round(BIC(ologit8),1)),c("AIC", round(AIC(ologit1),1), round(AIC(ologit2),1), round(AIC(ologit3),1), round(AIC(ologit4),1), round(AIC(ologit5),1), round(AIC(ologit6),1), round(AIC(ologit7),1), round(AIC(ologit8),1))))

# dane dataset

ologit1 <- polr(legStat~priOfJoi+odM+cannUse+opoiPres+tedsAM+tedsAP100k+arriv+vc100k+pc100k+govern, data=dane, Hess=TRUE)
#coeftest(ologit1)
ologit2 <- polr(legStat~log(priOfJoi)+odM+cannUse+opoiPres+tedsAM+log(tedsAP100k)+arriv+vc100k+pc100k+govern, data=dane, Hess=TRUE)
#coeftest(ologit2)
ologit3 <- polr(legStat~priOfJoi+log(odM)+cannUse+opoiPres+tedsAM+tedsAP100k+arriv+vc100k+pc100k+govern, data=dane, Hess=TRUE)
#coeftest(ologit3)
ologit4 <- polr(legStat~priOfJoi+odM+cannUse+log(opoiPres)+tedsAM+tedsAP100k+arriv+vc100k+pc100k+govern, data=dane, Hess=TRUE)
#coeftest(ologit4)
ologit5 <- polr(legStat~priOfJoi+odM+cannUse+opoiPres+tedsAM+tedsAP100k+arriv+log(vc100k)+log(pc100k)+govern, data=dane, Hess=TRUE)
#coeftest(ologit5)
ologit6 <- polr(legStat~priOfJoi+odM+cannUse+opoiPres+tedsAM+tedsAP100k+log(arriv)+vc100k+pc100k+govern, data=dane, Hess=TRUE)
#coeftest(ologit6)
ologit7 <- polr(legStat~log(priOfJoi)+log(odM)+cannUse+log(opoiPres)+tedsAM+log(tedsAP100k)+log(arriv)+log(vc100k)+log(pc100k)+govern, data=dane, Hess=TRUE)
#coeftest(ologit7)
ologit8 <- polr(legStat~priOfJoi+log(odM)+cannUse+log(opoiPres)+tedsAM+log(tedsAP100k)+log(arriv)+log(vc100k)+pc100k+govern, data=dane, Hess=TRUE)
#coeftest(ologit8)

stargazer(ologit1, ologit2, ologit3, ologit4, ologit5, ologit6, ologit7, ologit8, type="text", title="Models Predicting legStat",add.lines=list(c("BIC", round(BIC(ologit1),1), round(BIC(ologit2),1), round(BIC(ologit3),1), round(BIC(ologit4),1), round(BIC(ologit5),1), round(BIC(ologit6),1), round(BIC(ologit7),1), round(BIC(ologit8),1)),c("AIC", round(AIC(ologit1),1), round(AIC(ologit2),1), round(AIC(ologit3),1), round(AIC(ologit4),1), round(AIC(ologit5),1), round(AIC(ologit6),1), round(AIC(ologit7),1), round(AIC(ologit8),1))))




#######################Unspecified models#######################
# Linear model
reg <- lm(legStat~priOfJoi+odM+cannUse+opoiPres+tedsAM+tedsAP100k+arriv+vc100k+pc100k+govern, data=dane2)
summary(reg)
coeftest(reg)

# estimated on data2, where legStat is not set as a categorical variable, otherwise model estimation fails


# Logit
ologit <- polr(legStat~priOfJoi+odM+cannUse+opoiPres+tedsAM+tedsAP100k+arriv+vc100k+pc100k+govern, data=dane, Hess=TRUE)
coeftest(ologit)

# Probit
oprobit <- polr(legStat~priOfJoi+odM+cannUse+opoiPres+tedsAM+tedsAP100k+arriv+vc100k+pc100k+govern, data=dane, Hess=TRUE, method = "probit")


stargazer(ologit1, ologit2, ologit3, ologit4, ologit5, ologit6, ologit7, ologit8, type="text", title="Models Predicting legStat",add.lines=list(c("BIC", round(BIC(ologit1),1), round(BIC(ologit2),1), round(BIC(ologit3),1), round(BIC(ologit4),1), round(BIC(ologit5),1), round(BIC(ologit6),1), round(BIC(ologit7),1), round(BIC(ologit8),1)),c("AIC", round(AIC(ologit1),1), round(AIC(ologit2),1), round(AIC(ologit3),1), round(AIC(ologit4),1), round(AIC(ologit5),1), round(AIC(ologit6),1), round(AIC(ologit7),1), round(AIC(ologit8),1))))

# without logs
ologit1 <- polr(legStat~govern+cannUse+priOfJoi+odM+opoiPres+arriv+tedsAM+tedsAP100k+pc100k+vc100k, data=dane, Hess=TRUE)
coeftest(ologit1)

# log(everything but govern)
ologit2 <- polr(legStat~govern+log(cannUse)+log(priOfJoi)+log(odM)+log(opoiPres)+log(arriv)+log(tedsAM)+log(tedsAP100k)+log(pc100k)+log(vc100k), data=dane, Hess=TRUE)
coeftest(ologit2)

# log(cannUse)
ologit3 <- polr(legStat~govern+log(cannUse)+priOfJoi+odM+opoiPres+arriv+tedsAM+tedsAP100k+pc100k+vc100k, data=dane, Hess=TRUE)
coeftest(ologit3)

# log(cannUse) and log(odM)
ologit4 <- polr(legStat~govern+log(cannUse)+priOfJoi+log(odM)+opoiPres+arriv+tedsAM+tedsAP100k+pc100k+vc100k, data=dane, Hess=TRUE)
coeftest(ologit4)

# log(cannUse) and log(odM) and log(opoiPres)
ologit5 <- polr(legStat~govern+log(cannUse)+priOfJoi+log(odM)+log(opoiPres)+arriv+tedsAM+tedsAP100k+pc100k+vc100k, data=dane, Hess=TRUE)
coeftest(ologit5)

# log(cannUse) and log(odM) and log(opoiPres) and log(tedsAP100k)
ologit6 <- polr(legStat~govern+log(cannUse)+priOfJoi+log(odM)+log(opoiPres)+arriv+tedsAM+log(tedsAP100k)+pc100k+vc100k, data=dane, Hess=TRUE)
coeftest(ologit6)

# log(cannUse) and log(odM) and log(opoiPres) and log(tedsAM)
ologit7 <- polr(legStat~govern+log(cannUse)+priOfJoi+log(odM)+log(opoiPres)+arriv+log(tedsAM)+tedsAP100k+pc100k+vc100k, data=dane, Hess=TRUE)
coeftest(ologit7)

# log(cannUse) and log(odM) and log(opoiPres) and log(arriv)
ologit8 <- polr(legStat~govern+log(cannUse)+priOfJoi+log(odM)+log(opoiPres)+log(arriv)+tedsAM+tedsAP100k+pc100k+vc100k, data=dane, Hess=TRUE)
coeftest(ologit8)

# log(cannUse) and log(prOfJoi) and log(odM) and log(opoiPres) and log(arriv)
ologit9 <- polr(legStat~govern+log(cannUse)+log(priOfJoi)+log(odM)+log(opoiPres)+log(arriv)+tedsAM+tedsAP100k+pc100k+vc100k, data=dane, Hess=TRUE)
coeftest(ologit9)

# log(cannUse) and log(prOfJoi) and log(opoiPres) and log(arriv)
ologit10 <- polr(legStat~govern+log(cannUse)+log(priOfJoi)+odM+log(opoiPres)+log(arriv)+tedsAM+tedsAP100k+pc100k+vc100k, data=dane, Hess=TRUE)
coeftest(ologit10)

# log(cannUse) and log(odM) and log(opoiPres) and log(pc100k)
ologit11 <- polr(legStat~govern+log(cannUse)+priOfJoi+log(odM)+log(opoiPres)+arriv+tedsAM+tedsAP100k+log(pc100k)+vc100k, data=dane, Hess=TRUE)
coeftest(ologit11)

# log(cannUse) and log(odM) and log(opoiPres) and log(vc100k)
ologit12 <- polr(legStat~govern+log(cannUse)+priOfJoi+log(odM)+log(opoiPres)+arriv+tedsAM+tedsAP100k+pc100k+log(vc100k), data=dane, Hess=TRUE)
coeftest(ologit12)

# log(cannUse) and log(odM) and log(opoiPres) and log(pc100k) i log(vc100k)
ologit13 <- polr(legStat~govern+log(cannUse)+priOfJoi+log(odM)+log(opoiPres)+arriv+tedsAM+tedsAP100k+log(pc100k)+log(vc100k), data=dane, Hess=TRUE)
coeftest(ologit13)

# log(cannUse) and log(odM) and log(opoiPres) and log(arriv)
ologit8 <- polr(legStat~govern+log(cannUse)+priOfJoi+log(odM)+log(opoiPres)+log(arriv)+tedsAM+tedsAP100k+pc100k+vc100k, data=dane, Hess=TRUE)
coeftest(ologit8)

stargazer(ologit1, ologit2, ologit3, ologit4, ologit5, ologit6, ologit7, ologit8, ologit9, ologit10, ologit11, ologit12, ologit13, ologit14, ologit15, ologit16, ologit17, ologit18,
          type="text", title="Models Predicting legStat",
          add.lines=list(
            c("BIC", 
              round(BIC(ologit1),1), 
              round(BIC(ologit2),1), 
              round(BIC(ologit3),1), 
              round(BIC(ologit4),1), 
              round(BIC(ologit5),1), 
              round(BIC(ologit6),1), 
              round(BIC(ologit7),1), 
              round(BIC(ologit8),1),
              round(BIC(ologit9),1), 
              round(BIC(ologit10),1), 
              round(BIC(ologit11),1), 
              round(BIC(ologit12),1), 
              round(BIC(ologit13),1),
              round(BIC(ologit14),1), 
              round(BIC(ologit15),1), 
              round(BIC(ologit16),1), 
              round(BIC(ologit17),1),
              round(BIC(ologit18),1)
              ),
            c("AIC", 
              round(AIC(ologit1),1), 
              round(AIC(ologit2),1), 
              round(AIC(ologit3),1), 
              round(AIC(ologit4),1), 
              round(AIC(ologit5),1), 
              round(AIC(ologit6),1), 
              round(AIC(ologit7),1), 
              round(AIC(ologit8),1),
              round(AIC(ologit9),1), 
              round(AIC(ologit10),1), 
              round(AIC(ologit11),1), 
              round(AIC(ologit12),1), 
              round(AIC(ologit13),1),
              round(AIC(ologit14),1), 
              round(AIC(ologit15),1), 
              round(AIC(ologit16),1), 
              round(AIC(ologit17),1),
              round(AIC(ologit18),1)
              )
            ))


stargazer(ologit1, ologit2, ologit5, ologit9, ologit13, ologit14, ologit15, ologit16,
          type="text", title="Models Predicting legStat",
          add.lines=list(
            c("BIC", 
              round(BIC(ologit1),1), 
              round(BIC(ologit2),1), 
              round(BIC(ologit5),1), 
              round(BIC(ologit9),1), 
              round(BIC(ologit13),1),
              round(BIC(ologit14),1), 
              round(BIC(ologit15),1), 
              round(BIC(ologit16),1)
            ),
            c("AIC", 
              round(AIC(ologit1),1), 
              round(AIC(ologit2),1), 
              round(AIC(ologit5),1), 
              round(AIC(ologit9),1), 
              round(AIC(ologit13),1),
              round(AIC(ologit14),1), 
              round(AIC(ologit15),1), 
              round(AIC(ologit16),1)
            )
          ))


# another attemps

# log(cannUse) i log(odM) i log(opoiPres)
ologit5 <- polr(legStat~govern+log(cannUse)+priOfJoi+log(odM)+log(opoiPres)+arriv+tedsAM+tedsAP100k+pc100k+vc100k, data=dane, Hess=TRUE)
coeftest(ologit5)
# log(cannUse) i log(odM) i log(opoiPres) i log(arriv)
ologit8 <- polr(legStat~govern+log(cannUse)+priOfJoi+log(odM)+log(opoiPres)+log(arriv)+tedsAM+tedsAP100k+pc100k+vc100k, data=dane, Hess=TRUE)
coeftest(ologit8)
# log(odM) i log(opoiPres)
ologit14 <- polr(legStat~govern+cannUse+priOfJoi+log(odM)+log(opoiPres)+arriv+tedsAM+tedsAP100k+pc100k+vc100k, data=dane, Hess=TRUE)
coeftest(ologit14)
# log(odM) i log(opoiPres) i log(arriv)
ologit15 <- polr(legStat~govern+cannUse+priOfJoi+log(odM)+log(opoiPres)+log(arriv)+tedsAM+tedsAP100k+pc100k+vc100k, data=dane, Hess=TRUE)
coeftest(ologit15)
# log(priOfJoi) i log(odM) i log(opoiPres) i log(arriv)
ologit16 <- polr(legStat~govern+cannUse+log(priOfJoi)+log(odM)+log(opoiPres)+log(arriv)+tedsAM+tedsAP100k+pc100k+vc100k, data=dane, Hess=TRUE)
coeftest(ologit16)
# log(cannUse) i log(priOfJoi) i log(odM) i log(opoiPres) i log(arriv)
ologit17 <- polr(legStat~govern+log(cannUse)+log(priOfJoi)+log(odM)+log(opoiPres)+log(arriv)+tedsAM+tedsAP100k+pc100k+vc100k, data=dane, Hess=TRUE)
coeftest(ologit17)
# log(opoiPres) i log(arriv)
ologit18 <- polr(legStat~govern+cannUse+priOfJoi+odM+log(opoiPres)+log(arriv)+tedsAM+tedsAP100k+pc100k+vc100k, data=dane, Hess=TRUE)
coeftest(ologit18)


stargazer(ologit5, ologit8, ologit14, ologit15, ologit16, ologit17, ologit18, 
          type="text", title="Models Predicting legStat",
          add.lines=list(
            c("BIC", 
              round(BIC(ologit5),1), 
              round(BIC(ologit8),1), 
              round(BIC(ologit14),1), 
              round(BIC(ologit15),1), 
              round(BIC(ologit16),1), 
              round(BIC(ologit17),1),
              round(BIC(ologit18),1)
            ),
            c("AIC", 
              round(AIC(ologit5),1), 
              round(AIC(ologit8),1), 
              round(AIC(ologit14),1), 
              round(AIC(ologit15),1), 
              round(AIC(ologit16),1), 
              round(AIC(ologit17),1),
              round(AIC(ologit18),1)
            )
          ))


#######################Assumptions#######################

# Best model's number - 15
# Now I am going to compare linear regression, probit i logit with the same variables
# log(odM) i log(opoiPres) i log(arriv)


# Linear model
reg <- lm(legStat~govern+cannUse+priOfJoi+log(odM)+log(opoiPres)+log(arriv)+tedsAM+tedsAP100k+pc100k+vc100k, data=dane2)
summary(reg)
coeftest(reg)
# estimated on data2, where legStat is not set as a categorical variable, otherwise model estimation fails

# Logit
ologitC <- polr(legStat~govern+cannUse+priOfJoi+log(odM)+log(opoiPres)+log(arriv)+tedsAM+tedsAP100k+pc100k+vc100k, data=dane, Hess=TRUE)
coeftest(ologitC)
#probit
oprobitC <- polr(legStat~govern+cannUse+priOfJoi+log(odM)+log(opoiPres)+log(arriv)+tedsAM+tedsAP100k+pc100k+vc100k, data=dane, Hess=TRUE, method = "probit")
coeftest(oprobitC)

stargazer(reg, ologitC, oprobitC, 
          type="text", title="Models Predicting legStat",
          add.lines=list(
            c("BIC", 
              round(BIC(reg),1), 
              round(BIC(ologitC),1), 
              round(BIC(oprobitC),1)
            ),
            c("AIC", 
              round(AIC(reg),1), 
              round(AIC(ologitC),1), 
              round(AIC(oprobitC),1)
            )
          ))

# Logit model assumptions
ologitC <- polr(legStat~govern+cannUse+priOfJoi+log(odM)+log(opoiPres)+log(arriv)+tedsAM+tedsAP100k+pc100k+vc100k, data=dane, Hess=TRUE)
summary(ologitC)
coeftest(ologitC)

# Are variables jointly significant?
# Likelihood ratio test
ologitC.restricted = polr(legStat~1, data=dane)
lrtest(ologitC, ologitC.restricted)
# we can reject h0, that variables in the model are jointly insignificant

# Proper functional form
# goodness-of-fit tests

# to execute lipsitz's test I have to transform variables before model estimation
odM_log <- log(dane$odM)
opoiPres_log <- log(dane$opoiPres)
arriv_log <- log(dane$arriv)
ologitCL <- polr(legStat~govern+cannUse+priOfJoi+odM_log+opoiPres_log+arriv_log+tedsAM+tedsAP100k+pc100k+vc100k, data=dane, Hess=TRUE)
lipsitz.test(ologitCL, g=5)
# due to small sample I can't perform this test

# another tests
logitgof(dane$legStat, fitted(ologitC), g = 5, ord = TRUE)
pulkrob.chisq(ologitC, c("govern"))
pulkrob.deviance(ologitC, c("govern"))
# we cannot reject h0, that functional form is proper in each of the three tests above

# proportional odds assumption
brant(ologitCL)
# this test does not run correctly, the values of chi2 are equal to 0 for all variables


## Fit cumulative link model:
fm <- clm(legStat~govern+cannUse+priOfJoi+odM_log+opoiPres_log+arriv_log+tedsAM+tedsAP100k+pc100k+vc100k, data=dane)
summary(fm)
## test partial proportional odds assumption for temp and contact:
nominal_test(fm)
## no evidence of non-proportional odds.
## test if there are signs of scale effects:
scale_test(fm)
## no evidence of scale effects.

## tests of scale and nominal effects for the housing data from MASS:
if(require(MASS)) {
  fm1 <- clm(Sat ~ Infl + Type + Cont, weights = Freq, data = housing)
  scale_test(fm1)
  nominal_test(fm1)
  ## Evidence of multiplicative/scale effect of 'Cont'. This is a breach
  ## of the proportional odds assumption.
}

## Fit cumulative link model:
fm2 <- clm(rating ~ temp + contact, data=wine)
summary(fm2)
## test partial proportional odds assumption for temp and contact:
nominal_test(fm2)
## no evidence of non-proportional odds.
## test if there are signs of scale effects:
scale_test(fm)
## no evidence of scale effects.

# I cannot confirm all logit's assumptions (especially proportional odds assumption), so now I am gonna chech probit's assumptions

# Probit's assumptions
oprobitC <- polr(legStat~govern+cannUse+priOfJoi+log(odM)+log(opoiPres)+log(arriv)+tedsAM+tedsAP100k+pc100k+vc100k, data=dane, Hess=TRUE, method="probit")
summary(oprobitC)
coeftest(oprobitC)

# Are variables jointly significant?
# Likelihood ratio test
oprobitC.restricted = polr(legStat~1, data=dane)
lrtest(oprobitC, oprobitC.restricted)
# we can reject h0, that variables in the model are jointly insignificant

# Proper functional form
# goodness-of-fit tests

# to execute lipsitz's test I have to transform variables before model estimation
odM_log <- log(dane$odM)
opoiPres_log <- log(dane$opoiPres)
arriv_log <- log(dane$arriv)
oprobitCL <- polr(legStat~govern+cannUse+priOfJoi+odM_log+opoiPres_log+arriv_log+tedsAM+tedsAP100k+pc100k+vc100k, data=dane, Hess=TRUE, method="probit")
lipsitz.test(oprobitCL, g=5)
# and again due to small sample I can't perform this test

logitgof(dane$legStat, fitted(oprobitC), g = 5, ord = TRUE)
pulkrob.chisq(oprobitC, c("govern"))
pulkrob.deviance(oprobitC, c("govern"))
# we cannot reject h0, that functional form is proper in each of the three tests above

# So I confirm probit's assumptions and will stick with probit model for next steps

#######################General to specific modeling for logit model#######################

# Now I am going to perform general to specific modeling for probit model

# Step 1
# general model
ologitG1 <- polr(legStat~govern+cannUse+priOfJoi+log(odM)+log(opoiPres)+log(arriv)+tedsAM+tedsAP100k+pc100k+vc100k, data=dane, Hess=TRUE)
coeftest(ologitG1)
# test whether all insignificant variables all jointly insignificant
ologitG1a <- polr(legStat~govern+log(odM)+tedsAM+tedsAP100k+pc100k+vc100k, data=dane, Hess=TRUE)
anova(ologitG1, ologitG1a)
# all insignificant variables are jointly significant
# therefore we have to drop variables in the way one after another
# let's drop "the most insignificant" variable from ologitG1
# that is tedsAM

# Step 2
ologitG2 <- polr(legStat~govern+cannUse+priOfJoi+log(odM)+log(opoiPres)+log(arriv)+tedsAP100k+pc100k+vc100k, data=dane, Hess=TRUE)
coeftest(ologitG2)
# there are still insignificant variables in ologitG2 model
# let's drop "the most insignificant" variable from ologitG2
# that is govern
# Can we?
# let's check
# let's estimate model ologitG2 without govern
# and test joint hypothesis: beta_tedsAM=beta_govern=0
# in the general model that is model ologitG1
ologitG2a <- polr(legStat~cannUse+priOfJoi+log(odM)+log(opoiPres)+log(arriv)+tedsAP100k+pc100k+vc100k, data=dane, Hess=TRUE)
coeftest(ologitG2a)
anova(ologitG1,ologitG2a)
# we cannot reject the null, so govern might be dropped from ologitG2 model

# Step 3
ologitG3 <- polr(legStat~cannUse+priOfJoi+log(odM)+log(opoiPres)+log(arriv)+tedsAP100k+pc100k+vc100k, data=dane, Hess=TRUE)
coeftest(ologitG3)
# we would like to drop tedsAP100k from ologitG3
# to do so, we have to verify joint hypothesis that
# beta_tedsAM=beta_govern=beta_tedsAP100k=0
ologitG3a <- polr(legStat~cannUse+priOfJoi+log(odM)+log(opoiPres)+log(arriv)+pc100k+vc100k, data=dane, Hess=TRUE)
coeftest(ologitG3a)
anova(ologitG1,ologitG3a)
# we cannot reject the null, so tedsAP100k might be dropped from ologitG3 model

# Step 4
ologitG4 <- polr(legStat~cannUse+priOfJoi+log(odM)+log(opoiPres)+log(arriv)+pc100k+vc100k, data=dane, Hess=TRUE)
coeftest(ologitG4)
# we would like to drop vc100k from ologitG4
# to do so, we have to verify joint hypothesis that
# beta_tedsAM=beta_govern=beta_tedsAP100k=beta_vc100k=0
ologitG4a <- polr(legStat~cannUse+priOfJoi+log(odM)+log(opoiPres)+log(arriv)+pc100k+vc100k, data=dane, Hess=TRUE)
coeftest(ologitG4a)
anova(ologitG1,ologitG4a)
# we cannot reject the null, so vc100k might be dropped from ologitG4 model

# Step 5
ologitG5 <- polr(legStat~cannUse+priOfJoi+log(odM)+log(opoiPres)+log(arriv)+pc100k, data=dane, Hess=TRUE)
coeftest(ologitG5)
# we would like to drop log(odM) from ologitG5
# to do so, we have to verify joint hypothesis that
# beta_tedsAM=beta_govern=beta_tedsAP100k=beta_vc100k=-log(odM)=0
ologitG5a <- polr(legStat~cannUse+priOfJoi+log(opoiPres)+log(arriv)+pc100k, data=dane, Hess=TRUE)
coeftest(ologitG5)
anova(ologitG1,ologitG5a)
# we cannot reject the null, so log(odM) might be dropped from ologitG5 model

#Step 6
ologitG6 <- polr(legStat~cannUse+priOfJoi+log(opoiPres)+log(arriv)+pc100k, data=dane, Hess=TRUE)
coeftest(ologitG6)
# All variables are significant in this step, so
# this ends general-to-specific procedure.

#######################General to specific modeling for probit model#######################

# Now I am going to perform general to specific modeling for probit model

# Step 1
# general model
oprobitG1 <- polr(legStat~govern+cannUse+priOfJoi+log(odM)+log(opoiPres)+log(arriv)+tedsAM+tedsAP100k+pc100k+vc100k, data=dane, Hess=TRUE, method="probit")
coeftest(oprobitG1)
# test whether all insignificant variables all jointly insignificant
oprobitG1a <- polr(legStat~govern+log(odM)+tedsAM+tedsAP100k+pc100k+vc100k, data=dane, Hess=TRUE, method="probit")
anova(oprobitG1, oprobitG1a)
# all insignificant variables are jointly significant
# therefore we have to drop variables in the way one after another
# let's drop "the most insignificant" variable from oprobitG1
# that is tedsAM

# Step 2
oprobitG2 <- polr(legStat~govern+cannUse+priOfJoi+log(odM)+log(opoiPres)+log(arriv)+tedsAP100k+pc100k+vc100k, data=dane, Hess=TRUE, method="probit")
coeftest(oprobitG2)
# there are still insignificant variables in oprobitG2 model
# let's drop "the most insignificant" variable from oprobitG2
# that is govern
# Can we?
# let's check
# let's estimate model oprobitG2 without govern
# and test joint hypothesis: beta_tedsAM=beta_govern=0
# in the general model that is model ologitG1
oprobitG2a <- polr(legStat~cannUse+priOfJoi+log(odM)+log(opoiPres)+log(arriv)+tedsAP100k+pc100k+vc100k, data=dane, Hess=TRUE, method="probit")
coeftest(oprobitG2a)
anova(oprobitG2,oprobitG2a)
# we cannot reject the null, so govern might be dropped from oprobitG2 model

# Step 3
oprobitG3 <- polr(legStat~cannUse+priOfJoi+log(odM)+log(opoiPres)+log(arriv)+tedsAP100k+pc100k+vc100k, data=dane, Hess=TRUE, method="probit")
coeftest(oprobitG3)
# we would like to drop tedsAP100k from ologitG3
# to do so, we have to verify joint hypothesis that
# beta_tedsAM=beta_govern=beta_tedsAP100k=0
oprobitG3a <- polr(legStat~cannUse+priOfJoi+log(odM)+log(opoiPres)+log(arriv)+pc100k+vc100k, data=dane, Hess=TRUE, method="probit")
coeftest(oprobitG3a)
anova(oprobitG3,oprobitG3a)
# we cannot reject the null, so tedsAP100k might be dropped from ologitG3 model

# Step 4
oprobitG4 <- polr(legStat~cannUse+priOfJoi+log(odM)+log(opoiPres)+log(arriv)+pc100k+vc100k, data=dane, Hess=TRUE, method="probit")
coeftest(oprobitG4)
# we would like to drop vc100k from ologitG4
# to do so, we have to verify joint hypothesis that
# beta_tedsAM=beta_govern=beta_tedsAP100k=beta_vc100k=0
oprobitG4a <- polr(legStat~cannUse+priOfJoi+log(odM)+log(opoiPres)+log(arriv)+pc100k, data=dane, Hess=TRUE, method="probit")
coeftest(oprobitG4a)
anova(oprobitG4,oprobitG4a)
# we cannot reject the null, so vc100k might be dropped from ologitG4 model

#Step 5
oprobitG5 <- polr(legStat~cannUse+priOfJoi+log(odM)+log(opoiPres)+log(arriv)+pc100k, data=dane, Hess=TRUE, method="probit")
coeftest(oprobitG5)
# All variables are significant in this step, so
# this ends general-to-specific procedure.

# wywalenie odM
# Step 5
oprobitG5 <- polr(legStat~cannUse+priOfJoi+log(odM)+log(opoiPres)+log(arriv)+pc100k, data=dane, Hess=TRUE, method="probit")
coeftest(oprobitG5)
# we would like to drop log(odM) from ologitG5
# to do so, we have to verify joint hypothesis that
# beta_tedsAM=beta_govern=beta_tedsAP100k=beta_vc100k=-log(odM)=0
oprobitG5a <- polr(legStat~cannUse+priOfJoi+log(opoiPres)+log(arriv)+pc100k, data=dane, Hess=TRUE, method="probit")
coeftest(oprobitG5a)
anova(oprobitG5,oprobitG5a)
# we cannot reject the null, so log(odM) might be dropped from ologitG5 model

#Step 6
oprobitG6 <- polr(legStat~cannUse+priOfJoi+log(opoiPres)+log(arriv)+pc100k, data=dane, Hess=TRUE, method="probit")
coeftest(oprobitG6)
# All variables are significant in this step, so
# this ends general-to-specific procedure.

stargazer(oprobitG1, oprobitG2, oprobitG3, oprobitG4, oprobitG5, type="text")
          #, title="Models Predicting legStat"),
          #add.lines=list(
          #  c("BIC", 
          #    round(BIC(ologit1),1), 
          #    round(BIC(ologit2),1), 
          #    round(BIC(ologit5),1), 
          #    round(BIC(ologit9),1), 
          #    round(BIC(ologit13),1),
          #    round(BIC(ologit14),1), 
          #    round(BIC(ologit15),1), 
          #    round(BIC(ologit16),1)
          #  ),
          #  c("AIC", 
          #    round(AIC(ologit1),1), 
          #    round(AIC(ologit2),1), 
          #    round(AIC(ologit5),1), 
          #    round(AIC(ologit9),1), 
          #    round(AIC(ologit13),1),
          #    round(AIC(ologit14),1), 
          #    round(AIC(ologit15),1), 
          #    round(AIC(ologit16),1)
          #  )
          #))

summary(oprobitG5)
#######################Estimation of the effects#######################

oprobitG5 <- polr(legStat~cannUse+priOfJoi+log(odM)+log(opoiPres)+log(arriv)+pc100k, data=dane, Hess=TRUE, method="probit")
coeftest(oprobitG5)

# Estimate ordered logit for hstatus.
oprobitreg <- oprobit.reg(legStat~cannUse+priOfJoi+log(odM)+log(opoiPres)+log(arriv)+pc100k, data=dane)
summary(oprobitreg)

#Compute confusion table and misclassification error
legStatPredict = predict(oprobitG5,dane)
table(dane$legStat, legStatPredict)
mean(as.character(dane$legStat) != as.character(legStatPredict))

#Plotting the effects 
Effect(focal.predictors = "priOfJoi",oprobitG5)
plot(Effect(focal.predictors = "priOfJoi",oprobitG5))
plot(Effect(focal.predictors = c("cannUse", "priOfJoi"),ologit))

# marginal effects
options(scipen=999)
margins.oglmx(oprobitreg)


# marginal effects for a user-defined characteristics
source("ome.R")
# this function works with polr models
model = polr(health~income+female+num, data=rd, method="logistic")
summary(model)
oprobitG5
x = c(mean(dane$cannUse), mean(dane$priOfJoi), mean(log(dane$odM)), mean(log(dane$opoiPres)), mean(log(dane$arriv)), mean(dane$pc100k))
ome(oprobitG5, x)

x = c(7000, 0, 4)
ome(model, x)


#######################Automatic search for an optimal model#######################

# As a exercise I will try to build function to automatize proccess of searching for optimal model

# create a NULL vector called model so we have something to add our layers to
model <- NULL

# create a vector of the dataframe column names used to build the formula
vars <- names(dane)

# remove variable names you don’t want to use (at least
# the response variable (if its in the first column)
vars <- vars[-1]
vars <- vars[-1]
# removing also govern variable, because i dont want to log it
#vars <- vars[-10]

# adding logs
logs <- c("log(cannUse)","log(priOfJoi)","log(odM)","log(opoiPres)","log(arriv)","log(tedsAM)","log(tedsAP100k)","log(pc100k)","log(vc100k)")
vars <- append(vars, logs, after=length(vars))
vars

# the combn function will run every different combination of variables and then run the logit with const govern
for(i in 1:length(vars)){
  xx = combn(vars,i)
  if(is.null(dim(xx))){
    fla = paste("legStat ~ govern +", paste(xx, collapse="+"))
    model[[length(model)+1]]=polr(as.formula(fla),data=dane)
  } else {
    for(j in 1:dim(xx)[2]){
      fla = paste("legStat ~ govern +", paste(xx[1:dim(xx)[1],j], collapse="+"))
      model[[length(model)+1]]=polr(as.formula(fla),data=dane) 
    }
  }
}

# the combn function will run every different combination of variables and then run the logit without const govern
for(i in 1:length(vars)){
  xx = combn(vars,i)
  if(is.null(dim(xx))){
    fla = paste("legStat ~", paste(xx, collapse="+"))
    model[[length(model)+1]]=polr(as.formula(fla),data=dane)
  } else {
    for(j in 1:dim(xx)[2]){
      fla = paste("legStat ~", paste(xx[1:dim(xx)[1],j], collapse="+"))
      model[[length(model)+1]]=polr(as.formula(fla),data=dane) 
    }
  }
}


# see how many models were build using the loop above
length(model)

# create a vector to extract AIC and BIC values from the model variable
AICs <- NULL
BICs <- NULL
for(i in 1:length(model)){
  AICs[i] <- AIC(model[[i]])
  BICs[i] <- BIC(model[[i]])
}

# see which models were chosen as best by both methods
which(AICs==min(AICs))
which(BICs==min(BICs))

model[28] # without logs, const govern
model[31] # without logs, without const govern

model[81443] # with logs, 94331 combinations, AIC - cannUse arriv pc100k log(cannUse) log(priOfJoi) log(odM) log(arriv)
model[529] # with logs, 94331 combinations, BIC

test1 <- polr(legStat ~ cannUse + log(cannUse) + arriv + log(arriv) + log(odM) + log(priOfJoi) + pc100k, data=dane)
coeftest(testl)
BIC(test1)
AIC(test1)

test2 <- polr(legStat ~ cannUse + arriv + log(arriv), data=dane)
coeftest(test2)
BIC(test2)
AIC(test2)

# I made an assumption that cannUse variable has to be in the model, due to fact it was highly significant and by adopting this assumption I reduce number of combination I have to check
model <- NULL
vars1 <- names(dane)
vars1 <- vars1[-1]
vars1 <- vars1[-1]
vars1 <- vars1[-2]

# adding logs
logs1 <- c("log(priOfJoi)","log(odM)","log(opoiPres)","log(arriv)","log(tedsAM)","log(tedsAP100k)","log(pc100k)","log(vc100k)")
vars1 <- append(vars1, logs1, after=length(vars1))
vars1

# the combn function will run every different combination of variables and then run the logit with const govern
for(i in 1:length(vars1)){
  xx = combn(vars1,i)
  if(is.null(dim(xx))){
    fla = paste("legStat ~ cannUse +", paste(xx, collapse="+"))
    model[[length(model)+1]]=polr(as.formula(fla),data=dane)
  } else {
    for(j in 1:dim(xx)[2]){
      fla = paste("legStat ~ cannUse +", paste(xx[1:dim(xx)[1],j], collapse="+"))
      model[[length(model)+1]]=polr(as.formula(fla),data=dane) 
    }
  }
}

# see how many models were build using the loop above
length(model)

# create a vector to extract AIC and BIC values from the model variable
AICs <- NULL
BICs <- NULL
for(i in 1:length(model)){
  AICs[i] <- AIC(model[[i]])
  BICs[i] <- BIC(model[[i]])
}

# see which models were chosen as best by both methods
which(AICs==min(AICs))
which(BICs==min(BICs))

model[8429] # with logs, 25323 combinations, AIC - cannUse arriv pc100k log(cannUse) log(priOfJoi) log(odM) log(arriv)
model[83] # with logs, 25323 combinations, BIC

test3 <- polr(legStat ~ cannUse + arriv + log(arriv) + log(odM) + log(priOfJoi) + pc100k, data=dane)
coeftest(test3)
BIC(test3)
AIC(test3)

test4 <- polr(legStat ~ cannUse + arriv + log(arriv), data=dane)
coeftest(test4)
BIC(test4)
AIC(test4)

stargazer(test1, test2, test3, test4, 
          type="text", title="Models Predicting legStat",
          add.lines=list(
            c("BIC", 
              round(BIC(test1),1), 
              round(BIC(test2),1), 
              round(BIC(test3),1), 
              round(BIC(test4),1)
            ),
            c("AIC", 
              round(AIC(test1),1), 
              round(AIC(test2),1), 
              round(AIC(test3),1), 
              round(AIC(test4),1)
            )
          ))

model <- NULL
vars1 <- names(dane)
vars1 <- vars1[-1]
vars1 <- vars1[-1]
vars1 <- vars1[-2]
vars1 <- vars1[-5]
# adding logs
logs1 <- c("log(priOfJoi)","log(odM)","log(opoiPres)","log(tedsAM)","log(tedsAP100k)","log(pc100k)","log(vc100k)")
vars1 <- append(vars1, logs1, after=length(vars1))
vars1

for(i in 1:length(vars1)){
  xx = combn(vars1,i)
  if(is.null(dim(xx))){
    fla = paste("legStat ~ cannUse + arriv +", paste(xx, collapse="+"))
    model[[length(model)+1]]=polr(as.formula(fla),data=dane)
  } else {
    for(j in 1:dim(xx)[2]){
      fla = paste("legStat ~ cannUse + arriv +", paste(xx[1:dim(xx)[1],j], collapse="+"))
      model[[length(model)+1]]=polr(as.formula(fla),data=dane) 
    }
  }
}

# see how many models were build using the loop above
length(model)

# create a vector to extract AIC and BIC values from the model variable
AICs <- NULL
BICs <- NULL
for(i in 1:length(model)){
  AICs[i] <- AIC(model[[i]])
  BICs[i] <- BIC(model[[i]])
}

# see which models were chosen as best by both methods
which(AICs==min(AICs))
which(BICs==min(BICs))

model[4]

oprobitG5

test2 <- polr(legStat ~ cannUse + arriv + log(arriv), data=dane, method = "probit")
stargazer(oprobitG5, test2,
          type="text", title="Models Predicting legStat",
          add.lines=list(
            c("BIC", 
              round(BIC(oprobitG5),1), 
              round(BIC(test2),1)
            ),
            c("AIC", 
              round(AIC(oprobitG5),1), 
              round(AIC(test2),1)
            )
          ))
