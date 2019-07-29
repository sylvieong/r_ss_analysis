
# location (. points to the current working directory) and name of the data file
# REPLACE with the location and name of your data file
data_path = "./car_road_test.csv"

print("Reading data from file:", quote=FALSE)
print(data_path)
writeLines("\n")

# read csv file into a data frame
df <- read.table(data_path, header=TRUE, sep=",", quote="\"") 

print("Number of rows and columns of input table:", quote=FALSE) 
print(dim(df))
writeLines("\n")

# opens a new window and displays spreadsheet-like view of the data frame
View(df)    

# the "am" column is originally coded numerically, map it to coding by categories
# REPLACE "am" with the name of the column to be mapped in your dataset
# REPLACE c(-1, 0, 1) with TODO!!!
# REPLACE c("automatic", "manual") with the categories you want to map to
df$amCoded <- cut(df$am, breaks = c(-1, 0, 1), labels = c("automatic", "manual"))

# the "vs" column is originally coded numerically, map it to coding by categories
df$vsCoded <- cut(df$vs, breaks = c(-1, 0, 1), labels = c("v-shaped", "straight"))

# chi test: am, vs
tbl <- table(df$vsCoded, df$amCoded) 

chi2 <- chisq.test(tbl, correct=F)
p <- chi2$p.value
v <- sqrt(as.numeric(chi2$statistic) / sum(tbl))

# display the table and stats
print(tbl)

print("p-value:", quote=FALSE)
print(p)

print("Cramer's V (the smaller v, the lower the correlation):", quote=FALSE)
print(v)
writeLines("\n")

library(ggplot2)

# bar plots - x: vs, by: am
df_tbl <- as.data.frame(tbl)

# counts
dev.new()
plot(ggplot(df_tbl, aes(fill=df_tbl$Var2, y=df_tbl$Freq, x=df_tbl$Var1)) + 
    geom_bar(position="dodge", stat="identity") + scale_fill_brewer(palette = "Set2")) 

# percentages
dev.new()
plot(ggplot(df_tbl, aes(fill=df_tbl$Var2, y=df_tbl$Freq, x=df_tbl$Var1)) + 
    geom_bar( stat="identity", position="fill") + scale_fill_brewer(palette = "Set2")) 

