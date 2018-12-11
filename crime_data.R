#Set the working director in order to import data
setwd('/Users/erinstafford/Documents/Github/Linear_Models_Final') 

#You will probably have a different file path 

#read the data into R
crime_data = read.csv("communities.txt")

#ignoring location information and columns with more than 1000 instances missing
crime_data = cbind(crime_data[-c(1:5,127,125:122,118:102,129)])

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
list_attr = cbind(crime_data$racepctblack, crime_data$pctWPubAsst, crime_data$MalePctDivorce, crime_data$FemalePctDiv,crime_data$TotalPctDiv,crime_data$PctIlleg,crime_data$racePctWhite,crime_data$PctFam2Par, crime_data$PctKids2Par, crime_data$PctYoungKids2Par, crime_data$PctTeen2Par)
for (i in 1:ncol(list_attr)){
  if (skewness(list_attr[,i]) > 0.5){
    list_attr[,i] = list_attr[,i]^(1/3)
  }
  hist(list_attr[,i])
}
 #the race data is still skewed but the rest looks good 
  
  