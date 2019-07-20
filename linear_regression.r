data_dir = "C:/Users/Janice_2/Documents/ss_analysis/"
data_file = "Sample_Dataset_2014c.csv"
data_path = paste(data_dir, data_file, sep = '')
print(data_path)

df_all <- read.table(data_path, header=TRUE, sep=",", quote="\"") # read csv file into a dataframe
print(dim(df_all))
View(df_all) 	# spreadsheet view
print(str(df_all))	# variable/column types 

df_regr <- df_all[,c('Height', 'Weight')] # extract 2 columns for regression analysis into a new dataframe 

print(summary(df_regr)) 	# basic statistics of extracted variables

print(cor(df_regr$Height, df_regr$Weight, use="complete.obs"))	# correlation coefficient between 2 variables, strength of relationship

linearMod <- lm(Weight ~ Height, data=df_regr)	# fit a linear regression model


print(linearMod) 	# coefficients of the fitted model

print(summary(linearMod)) 	# p-value and other goodness of fit measures
