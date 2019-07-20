data_dir = "C:/Users/Janice_2/Documents/ss_analysis/"
data_file = "Sample_Dataset_2014c.csv"
data_path = paste(data_dir, data_file, sep = '')
print("Reading data from file:")
print(data_path)
writeLines("\n")

df_all <- read.table(data_path, header=TRUE, sep=",", quote="\"") # read csv file into a dataframe
print("Number of rows and columns of input table:") 
print(dim(df_all))
writeLines("\n")
View(df_all) 	# spreadsheet view

df_regr <- df_all[,c('Height', 'Weight')] # extract 2 columns for regression analysis into a new dataframe 

print("Basic statistics of variables extracted for analysis:")
print(summary(df_regr)) 	# basic statistics of extracted variables
writeLines("\n")

print("Correlation coeff (R-value):")
print(cor(df_regr$Height, df_regr$Weight, use="complete.obs"))	# correlation coefficient between 2 variables, strength of relationship
writeLines("\n")

linearMod <- lm(Weight ~ Height, data=df_regr)	# fit a linear regression model

# TODO: get P values - statistical significance of the model
# TODO: get R2 value
# TODO: get pretty plots

