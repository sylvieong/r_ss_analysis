data_dir = "C:/Users/Janice_2/Documents/ss_analysis/"
data_file = "LFS-71M0001-E-2019-June_F1c.csv"
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


"Job tenure with current employer (months)"
"Usual hourly wages, employees only"

df_regr <- df_all[,c('TENURE', 'HRLYEARN')] # extract 2 columns for regression analysis into a new dataframe 

print("Basic statistics of variables extracted for analysis:", quote=FALSE)
print(summary(df_regr)) 	# basic statistics of extracted variables
writeLines("\n")

print("Correlation coefficient (R value):", quote=FALSE)
print(cor(df_regr$TENURE, df_regr$HRLYEARN, use="complete.obs"))	# correlation coefficient between 2 variables, strength of relationship
writeLines("\n")

linearMod <- lm(HRLYEARN ~ TENURE, data=df_regr)	# fit a linear regression model
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
# print(lm_stats$coefficients[2,4]) # also the p-value when 2-variable instead of multi=linear
writeLines("\n")


library(ggplot2)


dev.new()
# plot points with fitted linear model
print("Plotting fitted regression line with color...", quote=FALSE)
writeLines("\n")
plot(ggplot(df_all, aes(x=TENURE, y=HRLYEARN)) + geom_point(
    color="blue",
        fill="blue",
        shape=21,
        alpha=0.2,
        size=0.05,
        stroke = 2) +  geom_smooth(method=lm , color="green", se=FALSE))


# UNION is currently coded as numerical, map it to categorical
df_all$UNIONCoded = cut(df_all$UNION, breaks = c(0, 1, 3), labels = c("Union member", "Non-unionized"))


dev.new()
# plot points differentiate by UNION
print("Plotting, by union membership...", quote=FALSE)
writeLines("\n")
plot(ggplot(df_all, aes(x=TENURE, y=HRLYEARN, color=UNIONCoded, size=UNIONCoded)) +  
     geom_point(size=1, alpha=0.5))


# plots with density
print("Plotting densities...", quote=FALSE)
writeLines("\n")

dev.new()
plot(ggplot(df_all, aes(x=TENURE, y=HRLYEARN) ) +
  geom_bin2d(bins=100) +
  theme_bw() +
  scale_fill_distiller(palette= "Spectral", direction=1))


dev.new()
plot(ggplot(df_all, aes(x=TENURE, y=HRLYEARN) ) +
  stat_density_2d(aes(fill = ..level..), geom = "polygon") +
  scale_fill_distiller(palette= "Spectral", direction=1))

# box plots
print("Box plots...", quote=FALSE)
writeLines("\n")
 
dev.new()
split.screen( figs = c( 1, 2 ) )
screen(1)
boxplot(df_all$HRLYEARN, main="Hourly wages", sub=paste("Outlier rows: ", length(boxplot.stats(df_all$HRLYEARN)$out)))
screen(2)
boxplot(df_all$TENURE, main="Job tenure (months)", sub=paste("Outlier rows: ", length(boxplot.stats(df_all$TENURE)$out)))

dev.new()
plot(ggplot(df_all, aes(x=UNIONCoded, y=HRLYEARN, fill=UNIONCoded)) + 
    geom_boxplot(
        
        alpha=0.3,
        
        # custom outliers
        outlier.colour="red",
        outlier.size=2
    
    ) + theme(legend.position="none"))


dev.new()
plot(ggplot(df_all, aes(x=UNIONCoded, y=TENURE, fill=UNIONCoded)) + 
    geom_boxplot(
        
        alpha=0.3,
        
        # custom outliers
        outlier.colour="red",
        outlier.size=2
    
    ) + theme(legend.position="none"))
