# Location and name of the data file. Note that "./" points to the current working directory.
# TODO: Replace with the location and name of your data file.
data_path = "./car_road_test.csv"

print("Reading data from file:", quote=FALSE)
print(data_path)
writeLines("\n")

# A. Read csv file into the data frame df and view its contents.
df <- read.table(data_path, header=TRUE, sep=",", quote="\"")
print("Number of rows and columns of input table:", quote=FALSE) 
print(dim(df))
writeLines("\n")

# Open a new window and displays spreadsheet-like view of the data frame df.
View(df) 	

# B. Get basic statistics and perform regression. 
# Extract 2 columns (ratio variables)  into a new data frame df_regr.
# mpg is the dependent variable, hp is the independent variable.
# TODO: Replace mpg and hp with the columns (ratio variables) in your dataset that you want to analyze, 
# repeat for everywhere else that mpg and hp appears in this file. 
df_regr <- df[,c('mpg', 'hp')] 

print("Basic statistics of variables extracted for analysis:", quote=FALSE)
print(summary(df_regr)) 
writeLines("\n")

# Get correlation coefficient between 2 ratio variables.
print("Correlation coefficient (R-value):", quote=FALSE)
print(cor(df_regr$mpg, df_regr$hp, use="complete.obs"))	
writeLines("\n")

# Fit a linear regression model.
linearMod <- lm(mpg ~ hp, data=df_regr)	
lm_stats = summary(linearMod)

print("R-squared value:", quote=FALSE)
print(lm_stats$r.squared)
writeLines("\n")

# Get and display the p-value. 
f <- lm_stats$fstatistic
p <- pf(f[1],f[2],f[3],lower.tail=F)
attributes(p) <- NULL

print("p-value:", quote=FALSE)
print(p)
writeLines("\n")

# C. Map numerically coded nominal column to categorical coding. 
# The am column is originally coded numerically, map it to categorical values.
# A new column amCoded, is created in the data frame df. 
# The command maps:
# values in the range > -1 and <= 0 to "automatic"; 
# values in the range > 0 and <= 1  to "manual".
# TODO: Replace am with the name of the column to be mapped in your dataset.
# TODO: Replace -1, 0, 1 according to the numerical coding in the column to be mapped. 
# TODO: Replace "automatic", "manual" with the categorical values you want to map to.
# TODO: Replace amCoded with the name of the new column you want to create,
# repeat for everywhere else that amCoded appears in this file.
df$amCoded = cut(df$am, breaks = c(-1, 0, 1), labels = c("automatic", "manual"))

# Load the package ggplot2. 
library(ggplot2)

# D. Plots.

# Scatter plot and the fitted linear regression line.
dev.new() # Create a new window to plot in.
print("Plotting fitted regression line with color...", quote=FALSE)
writeLines("\n")
plot(ggplot(df, aes(x=hp, y=mpg)) + geom_point(
    color="blue",
        fill="blue",
        shape=21,
        alpha=0.2,
        size=0.05,
        stroke = 2) +  geom_smooth(method=lm , color="green", se=FALSE))

# Scatter plot differentiated by amCoded.
dev.new() # Create a new window to plot in.
print("Plotting, by transmission type...", quote=FALSE)
writeLines("\n")
plot(ggplot(df, aes(x=hp, y=mpg, color=amCoded, size=amCoded)) +  
     geom_point(size=1, alpha=0.5))

# Density plot
dev.new()
print("Plotting densities...", quote=FALSE)
writeLines("\n")
plot(ggplot(df, aes(x=hp, y=mpg) ) +
  stat_density_2d(aes(fill = ..level..), geom = "polygon") +
  scale_fill_distiller(palette= "Spectral", direction=1))

# Box plots
print("Box plots...", quote=FALSE)
writeLines("\n")

# Box plots for hp and mpg
dev.new()
split.screen( figs = c( 1, 2 ) )
screen(1)
boxplot(df$hp, main="hp", sub=paste("Outlier rows: ", length(boxplot.stats(df$hp)$out)))
screen(2)
boxplot(df$mpg, main="mpg", sub=paste("Outlier rows: ", length(boxplot.stats(df$mpg)$out)))

# Separate box plots for hp, one for each value of amCoded
dev.new()
plot(ggplot(df, aes(x=amCoded, y=hp, fill=amCoded)) + 
    geom_boxplot(
        alpha=0.3,
        # custom outliers
        outlier.colour="red",
        outlier.size=2
    ) + theme(legend.position="none"))

# Separate box plots for mpg, one for each value of amCoded
dev.new()
plot(ggplot(df, aes(x=amCoded, y=mpg, fill=amCoded)) + 
    geom_boxplot(
        alpha=0.3,
        # custom outliers
        outlier.colour="red",
        outlier.size=2
    ) + theme(legend.position="none"))
