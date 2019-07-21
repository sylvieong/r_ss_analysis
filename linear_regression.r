data_dir = "C:/Users/Janice_2/Documents/ss_analysis/"
data_file = "Sample_Dataset_2014c.csv"
data_path = paste(data_dir, data_file, sep = '')
writeLines("\n")
print("Reading data from file:", quote=FALSE)
print(data_path)
writeLines("\n")

df_all <- read.table(data_path, header=TRUE, sep=",", quote="\"") # read csv file into a dataframe
print("Number of rows and columns of input table:", quote=FALSE) 
print(dim(df_all))
writeLines("\n")
View(df_all) 	# spreadsheet view

df_regr <- df_all[,c('Height', 'Weight')] # extract 2 columns for regression analysis into a new dataframe 

print("Basic statistics of variables extracted for analysis:", quote=FALSE)
print(summary(df_regr)) 	# basic statistics of extracted variables
writeLines("\n")

print("Correlation coeff (R-value):", quote=FALSE)
print(cor(df_regr$Height, df_regr$Weight, use="complete.obs"))	# correlation coefficient between 2 variables, strength of relationship
writeLines("\n")

linearMod <- lm(Weight ~ Height, data=df_regr)	# fit a linear regression model

library(ggplot2)

# # basic plot with fitted regression line
# print("Plotting fitted regression line...", quote=FALSE)
# writeLines("\n")
# plot(ggplot(df_regr, aes(x=Height, y=Weight)) + geom_point(shape=1) +  geom_smooth(method=lm , color="green", se=FALSE))

dev.new()
# with a little more of color
print("Plotting fitted regression line with more color...", quote=FALSE)
writeLines("\n")
plot(ggplot(df_regr, aes(x=Height, y=Weight)) + geom_point(
    color="blue",
        fill="blue",
        shape=21,
        alpha=0.2,
        size=2,
        stroke = 2) +  geom_smooth(method=lm , color="green", se=FALSE))



# TODO: get P values - statistical significance of the model
# TODO: get R2 value

# Gender is currently coded as numerical (0 and 1), map it to categorical
df_all$GenderCoded = cut(df_all$Gender, breaks = c(-1, 0, 1), labels = c("Male", "Female"))

# Athete is currently coded as numerical (0 and 1), map it to categorical
df_all$AthleteCoded = cut(df_all$Athlete, breaks = c(-1, 0, 1), labels = c("No", "Yes"))

dev.new()
# plot points differentiate by gender
print("Plotting, by gender...", quote=FALSE)
writeLines("\n")
plot(ggplot(df_all, aes(x=Height, y=Weight, color=GenderCoded, size=GenderCoded)) +  
     geom_point(size=2, alpha=0.5))


dev.new()
# plot points differentiate by gender
print("Plotting, by athlete status...", quote=FALSE)
writeLines("\n")
plot(ggplot(df_all, aes(x=Height, y=Weight, color=AthleteCoded, size=AthleteCoded)) +  
     geom_point(size=2, alpha=0.5))


# TODO: other visualizations and data explorations get pretty plots

