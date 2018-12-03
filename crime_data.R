setwd('/Users/erinstafford/Desktop')
crime_data = read.csv("communities.txt")
#ignoring location information
crime_rate = crime_data[,128]
crime_data = crime_data[,6:127]
head(crime_data)

