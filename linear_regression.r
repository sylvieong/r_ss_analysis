mydata <- read.table("C:/Users/Janice_2/Documents/ss_analysis/Sample_Dataset_2014c.csv", header=TRUE, sep=",", quote="\"")
dim(mydata)

dfnew1 <- mydata[,c('Height', 'Weight')]

print(summary(dfnew1))

#Height          Weight     
# Min.   :55.00   Min.   :101.7  
# 1st Qu.:64.83   1st Qu.:153.9  
# Median :67.57   Median :173.0  
# Mean   :68.03   Mean   :181.0  
# 3rd Qu.:71.58   3rd Qu.:204.3  
# Max.   :84.41   Max.   :350.1  
# NA's   :27      NA's   :59
