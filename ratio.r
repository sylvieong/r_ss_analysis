
data_path = "./car_road_test.csv"
writeLines("\n")
print("Reading data from file:", quote=FALSE)
print(data_path)
writeLines("\n")

df_all <- read.table(data_path, header=TRUE, sep=",", quote="\"") # read csv file into a dataframe
print("Number of rows and columns of input table:", quote=FALSE) 
print(dim(df))
writeLines("\n")
View(df_all) 	# spreadsheet view


# extract 2 columns for regression analysis into a new dataframe 
df_regr <- df[,c('mpg', 'hp')] 

print("Basic statistics of variables extracted for analysis:", quote=FALSE)
print(summary(df_regr)) 	# basic statistics of extracted variables
writeLines("\n")

print("Correlation coefficient (R-value):", quote=FALSE)
print(cor(df_regr$mpg, df_regr$hp, use="complete.obs"))	# correlation coefficient between 2 variables, strength of relationship
writeLines("\n")

linearMod <- lm(mpg ~ hp, data=df_regr)	# fit a linear regression model
lm_stats = summary(linearMod)

print("R-squared value:", quote=FALSE)
print(lm_stats$r.squared)
writeLines("\n")

# get p values 
f <- lm_stats$fstatistic
p <- pf(f[1],f[2],f[3],lower.tail=F)
attributes(p) <- NULL

print("p-value:", quote=FALSE)
print(p)
writeLines("\n")

library(ggplot2)

dev.new()
# plot points with fitted linear model
print("Plotting fitted regression line with color...", quote=FALSE)
writeLines("\n")
plot(ggplot(df, aes(x=hp, y=mpg)) + geom_point(
    color="blue",
        fill="blue",
        shape=21,
        alpha=0.2,
        size=0.05,
        stroke = 2) +  geom_smooth(method=lm , color="green", se=FALSE))

# "am" is originally coded as numerical, map it to coding by categories
df$amCoded = cut(df$am, breaks = c(-1, 0, 1), labels = c("automatic", "manual"))

dev.new()
# plot points differentiate by transmission type
print("Plotting, by transmission type...", quote=FALSE)
writeLines("\n")
plot(ggplot(df, aes(x=hp, y=mpg, color=amCoded, size=amCoded)) +  
     geom_point(size=1, alpha=0.5))


# plots with density
print("Plotting densities...", quote=FALSE)
writeLines("\n")

# dev.new()
# plot(ggplot(df, aes(x=hp, y=mpg) ) +
#   geom_bin2d(bins=100) +
#   theme_bw() +
#   scale_fill_distiller(palette= "Spectral", direction=1))


dev.new()
plot(ggplot(df, aes(x=hp, y=mpg) ) +
  stat_density_2d(aes(fill = ..level..), geom = "polygon") +
  scale_fill_distiller(palette= "Spectral", direction=1))

# box plots
print("Box plots...", quote=FALSE)
writeLines("\n")
 
dev.new()
split.screen( figs = c( 1, 2 ) )
screen(1)
boxplot(df$hp, main="Horsepower", sub=paste("Outlier rows: ", length(boxplot.stats(df$hp)$out)))
screen(2)
boxplot(df$mpg, main="Miles per gallon", sub=paste("Outlier rows: ", length(boxplot.stats(df$mpg)$out)))

dev.new()
plot(ggplot(df, aes(x=amCoded, y=hp, fill=amCoded)) + 
    geom_boxplot(
        
        alpha=0.3,
        
        # custom outliers
        outlier.colour="red",
        outlier.size=2
    
    ) + theme(legend.position="none"))


dev.new()
plot(ggplot(df, aes(x=amCoded, y=mpg, fill=amCoded)) + 
    geom_boxplot(
        
        alpha=0.3,
        
        # custom outliers
        outlier.colour="red",
        outlier.size=2
    
    ) + theme(legend.position="none"))
