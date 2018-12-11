#Set the working director in order to import data
setwd('/Users/erinstafford/Documents/Github/Linear_Models_Final') 

#You will probably have a different file path 

#read the data into R
crime_data = read.csv("communities.txt")

#ignoring location information and columns with more than 1000 instances missing
crime_data = cbind(crime_data[-c(1:5,127,125:122,118:102,129)])

#not alot of missing points, so
crime_data =na.omit(crime_data)

#look at distribution of violent crime
hist(crime_data$ViolentCrimesPerPop) # this is not normally distributed - skewed

# further proof of skew
qqnorm(crime_data$ViolentCrimesPerPop) 
qqline(crime_data$ViolentCrimesPerPop)

#box plots can show outliers
boxplot(crime_data$ViolentCrimesPerPop) 
#apparently there are many outliers in this data set, all of them are > 0.7 ViolentCrimesPerPop
#These may be the points we most want to know about so dont throw them away

#To fix this problem - transform the data
#We do not need to scale the data because all attributes are alreay between 0 and 1
#Note: If skewness value lies above +1 or below -1, data is highly skewed. If it lies between +0.5 to -0.5, it is moderately skewed. If the value is 0, then the data is symmetric
library(e1071)
skewness(crime_data$ViolentCrimesPerPop) # highly skewed
#For right skewed data, common tansformations include square root, cube root, and log
hist(log(crime_data$ViolentCrimesPerPop))#still pretty skewed

hist(sqrt(crime_data$ViolentCrimesPerPop)) 
skewness(sqrt(crime_data$ViolentCrimesPerPop)) #still pretty skewed

hist((crime_data$ViolentCrimesPerPop)^(1/3)) 
skewness((crime_data$ViolentCrimesPerPop)^(1/3)) #moderately skewed, but looks the best

#apply transformation
crime_data$ViolentCrimesPerPop = (crime_data$ViolentCrimesPerPop)^(1/3)
#look at new distribution of violent crime
hist(crime_data$ViolentCrimesPerPop) # this is not normally distributed - skewed

#Much better
qqnorm(crime_data$ViolentCrimesPerPop) 
qqline(crime_data$ViolentCrimesPerPop)

#box plot shows no outliers!
boxplot(crime_data$ViolentCrimesPerPop) 

#look at correlations to see which attributes are most correlated with violont crime (vc)
c = cor(na.omit(crime_data))
corr_vc = c[101,]
length(which(corr_vc > 0.55)) 

#the values that satisfy which(corr_vc > 0.6) are the 7 most correlated (positive)
pairs(crime_data[which(corr_vc > 0.55)])

length(which(corr_vc < -0.65)) 
pairs(cbind(crime_data[which(corr_vc < -0.65)],crime_data$ViolentCrimesPerPop))
#the values that satisfy which(corr_vc < -0.65) are the 5 most correlated (negative)

#For now we will loot at
#racepctblack, pctWPubAsst, MalePctDivorce, FemalePctDiv,TotalPctDiv,PctIlleg +
#racePctWhite,PctFam2Par, PctKids2Par, PctYoungKids2Par, PctTeen2Par -

#These are probably highly correlated with each other, but we'll go back to that

#Are these highly correlated attributes normally distributed? -no
hist((crime_data$racepctblack)^(1/3))
skewness((crime_data$racepctblack)^(1/3)) #needed transformation

hist((crime_data$pctWPubAsst)) #need transformation
hist((crime_data$pctWPubAsst)^(1/3))
skewness((crime_data$pctWPubAsst)^(1/3))

hist((crime_data$MalePctDivorce)) #this one is fine
skewness((crime_data$MalePctDivorce))

#speed this process up
for (i in 1:ncol(crime_data)){
  if (skewness(crime_data[,i]) > 0.5){
    crime_data[,i] = crime_data[,i]^(1/3)
  }
  hist(crime_data[,i])
}
 #the race data is still skewed but the rest looks good

#Lets try a model using these factors
mod.most_cor_all = lm(ViolentCrimesPerPop~racepctblack+ pctWPubAsst+TotalPctDiv+MalePctDivorce+ FemalePctDiv+PctIlleg+PctFam2Par+racePctWhite+ PctKids2Par+ PctYoungKids2Par+ PctTeen2Par, data = crime_data)
plot(mod.most_cor_all)  #looks like there's some outliers, also large residuals
summary(mod.most_cor_all) #0.6548

#Looking at variance inflation factors
library(faraway)
vif(mod.most_cor_all)
X.e = na.omit(crime_data[,1:101])
d.svd.e = svd(X.e)$d
cond.index = max(d.svd.e)/d.svd.e
cond.num = max(d.svd.e)/min(d.svd.e)


#are the resdiuals normally distributed?
library(MASS)
qqnorm(studres(mod.most_cor_all))
qqline(studres(mod.most_cor_all))
shapiro.test(studres(mod.most_cor_all)) #they are significantly different from normal


#What happens when we remove 376,774,1231 (the outliers)
crime_data_rmo = crime_data[-c(376,774,1231),]
mod.most_cor_rm_outliers = lm(ViolentCrimesPerPop~racepctblack+ pctWPubAsst+TotalPctDiv+MalePctDivorce+ FemalePctDiv+PctIlleg+PctFam2Par+racePctWhite+ PctKids2Par+ PctYoungKids2Par+ PctTeen2Par, data = crime_data_rmo)
plot(mod.most_cor_rm_outliers)  #looks like there's some outliers, also large residuals
summary(mod.most_cor_rm_outliers)

#Does this improve the normality of the residuals?
qqnorm(studres(mod.most_cor_rm_outliers))
qqline(studres(mod.most_cor_rm_outliers))
shapiro.test(studres(mod.most_cor_rm_outliers)) #they are still significantly different from normal, but a bit better
#p-value = 2.714e-11

#high-leverage values: having the most impact on the model
plot(hatvalues(mod.most_cor_rm_outliers))
#identify(hatvalues(mod.most_cor))

#Lets remove these to see if the results are improved
crime_data_rmhl = crime_data_rmo[-c(1267,1454,948,141,1033,1634),]
mod.most_cor_rm_hatvals = lm(ViolentCrimesPerPop~racepctblack+ pctWPubAsst+TotalPctDiv+MalePctDivorce+ FemalePctDiv+PctIlleg+PctFam2Par+racePctWhite+ PctKids2Par+ PctYoungKids2Par+ PctTeen2Par, data = crime_data_rmhl)
plot(mod.most_cor_rm_hatvals,which =2)  #looks like there's some outliers, also large residuals
summary(mod.most_cor_rm_hatvals)
qqnorm(studres(mod.most_cor_rm_hatvals))
qqline(studres(mod.most_cor_rm_hatvals))
shapiro.test(studres(mod.most_cor_rm_hatvals)) #p-value = 2.607e-11, not much better than previous model
plot(hatvalues(mod.most_cor_rm_hatvals))
#identify(hatvalues(mod.most_cor_rm_hatvals))

#The R^2 values for these models are at most .6626, can we do better?


#So, lets see if the attributes we're using are really the best by doing PCA
crime.pca <- prcomp(na.omit(crime_data[,1:100]) )
print(crime.pca)
plot(crime.pca,type = "l")
summary(crime.pca)
crime.vars = crime_data[,1:100]
#biplot(crime.pca) #too many vars for this to help

#How many principal components are needed to provide a good summary of the data? 
var <- crime.pca$sdev^2
var.percent <- var/sum(var) * 100
barplot(var.percent, xlab='PC', ylab='Percent Variance', names.arg=1:length(var.percent), las=1, ylim=c(0, max(var.percent)), col='gray')
abline(h=1/ncol(crime_data[,1:100])*100, col='red')
sum(var.percent[1:14])
#according to this we need the first 14 principal components that account for 84.70506 percent of the variance

pca_crime = crime.pca$x
crime.mod.pca = lm(na.omit(crime_data)$ViolentCrimesPerPop~pca_crime)
summary(crime.mod.pca) #R^2: 0.7231 - better than model with most correlated variables
shapiro.test(studres(crime.mod.pca)) #1.432e-12 worse
qqnorm(studres(crime.mod.pca))
qqline(studres(crime.mod.pca))

#refit with good pcs only
crime.mod.pca_rm = lm(na.omit(crime_data)$ViolentCrimesPerPop~pca_crime[,-c(6,8,12,15:16,18:20,24,26:33,35:38,40,42:45,47:51,53,56:57,59,61,63,65:67,70:72,74,76:78,80:81,83:84,87:90,93,95,97,99:100)])
summary(crime.mod.pca_rm)
#R^2: 0.6528 - worse
shapiro.test(studres(crime.mod.pca_rm)) #7.27e-10 better
qqnorm(studres(crime.mod.pca_rm))
qqline(studres(crime.mod.pca_rm))

#Other things to try
plot(mod.most_cor_all$fitted,mod.most_cor_all$res)
lev.crime = hatvalues(mod.most_cor_all)
stdres.crime = rstandard(mod.most_cor_all)
plot(stdres.crime^2,lev.crime,xlab = 'squared standardized residual',ylab = 'leverage')
#identify(stdres.crime^2,lev.crime)
#worry about cases with pii >2p/N = 200/1994 = 0.1003009
lines(c(0,25),c(.1,.1)) #so we should be worried about 1270 and 1457

#remove these two points 
crime_data_rm_hl =crime_data[-c(1270,1457),]

#Now, lets try an AIC
crime.1 = lm(ViolentCrimesPerPop~1,data = crime_data_rm_hl)
crime.full = lm(ViolentCrimesPerPop~., data = crime_data_rm_hl)

crime.step.bf = stepAIC(crime.full,direction="both",scope = list(lower = crime.1,upper = crime.full))
summary(crime.step.bf) # R^2 = 0.7182, best
shapiro.test(studres(crime.step.bf)) #3.52e-12 worse
qqnorm(studres(crime.step.bf))
qqline(studres(crime.step.bf))
