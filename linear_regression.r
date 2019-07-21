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

print("Correlation coefficient (R value):", quote=FALSE)
print(cor(df_regr$Height, df_regr$Weight, use="complete.obs"))	# correlation coefficient between 2 variables, strength of relationship
writeLines("\n")

linearMod <- lm(Weight ~ Height, data=df_regr)	# fit a linear regression model
lm_stats = summary(linearMod)

print("R-squared value:", quote=FALSE)
print(lm_stats$r.squared)
writeLines("\n")

# get p values 
f <- lm_stats$fstatistic
p <- pf(f[1],f[2],f[3],lower.tail=F)

print("p-value:", quote=FALSE)
print(p)
# print(lm_stats$coefficients[2,4]) # also the p-value when 2-variable instead of multi=linear
writeLines("\n")


library(ggplot2)

dev.new()
# plot points with fitted linear model
print("Plotting fitted regression line with color...", quote=FALSE)
writeLines("\n")
plot(ggplot(df_all, aes(x=Height, y=Weight)) + geom_point(
    color="blue",
        fill="blue",
        shape=21,
        alpha=0.2,
        size=2,
        stroke = 2) +  geom_smooth(method=lm , color="green", se=FALSE))


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


# plots with density
print("Plotting densities...", quote=FALSE)
writeLines("\n")

dev.new()
plot(ggplot(df_all, aes(x=Height, y=Weight) ) +
  geom_bin2d() +
  theme_bw())

dev.new()
plot(ggplot(df_all, aes(x=Height, y=Weight) ) +
  geom_bin2d() +
  theme_bw() +
  scale_fill_distiller(palette= "Spectral", direction=1))


dev.new()
plot(ggplot(df_all, aes(x=Height, y=Weight) ) +
  stat_density_2d(aes(fill = ..level..), geom = "polygon") +
  scale_fill_distiller(palette= "Spectral", direction=1))

dev.new()
plot(ggplot(df_all, aes(x=Height, y=Weight) ) +
  stat_density_2d(aes(fill = ..level..), geom = "polygon")) 

dev.new()
plot <- ggplot(df_all, aes(x=Height, y=Weight))
plot(plot + stat_density2d(geom="tile", aes(fill = ..density..), contour = FALSE) + geom_point(colour = "white"))


# box plots
print("Box plots...", quote=FALSE)
writeLines("\n")
 
dev.new()
split.screen( figs = c( 1, 2 ) )
screen(1)
boxplot(df_all$Weight, main="Weight", sub=paste("Outlier rows: ", length(boxplot.stats(df_all$Weight)$out)))
screen(2)
boxplot(df_all$Height, main="Height", sub=paste("Outlier rows: ", length(boxplot.stats(df_all$Height)$out)))

dev.new()
plot(ggplot(df_all, aes(x=AthleteCoded, y=Weight, fill=AthleteCoded)) + 
    geom_boxplot(
        
        alpha=0.3,
        
        # custom outliers
        outlier.colour="red",
        outlier.size=2
    
    ) + theme(legend.position="none"))


dev.new()
plot(ggplot(df_all, aes(x=AthleteCoded, y=Height, fill=AthleteCoded)) + 
    geom_boxplot(
        
        alpha=0.3,
        
        # custom outliers
        outlier.colour="red",
        outlier.size=2
    
    ) + theme(legend.position="none"))
