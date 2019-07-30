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

# B. Map numerically coded nominal variable columns to categorical coding. 
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
df$amCoded <- cut(df$am, breaks = c(-1, 0, 1), labels = c("automatic", "manual"))

# The vs column is originally coded numerically, map it to categorical values.
# Description of command and TODOs are analogous to that for the am column.
# TODO: Replace vsCoded with the name of the new column you want to create, 
# repeat for everywhere else that vsCoded appears in this file.
df$vsCoded <- cut(df$vs, breaks = c(-1, 0, 1), labels = c("v-shaped", "straight"))

# C. Perform chi test between the columns (nomimal variables) amCoded and vsCoded.
# (1) Create a table populated with the co-occurence counts of values of amCoded and vsCoded, 
# i.e. the contingency table of amCoded and vsCoded.
tbl <- table(df$vsCoded, df$amCoded) 
# The content of tbl is:
#           automatic manual
#  v-shaped        12      6
#  straight         7      7

# (2) Apply Pearson’s Chi-squared test to the contingency table tbl.
chi2 <- chisq.test(tbl, correct=F)
# (3) Get the p-value of the test.
p <- chi2$p.value
# (4) Get the Cramer's V.
v <- sqrt(as.numeric(chi2$statistic) / sum(tbl))

# Print the contingency table tbl and stats.
print(tbl)

print("p-value:", quote=FALSE)
print(p)

print("Cramer's V (the smaller v, the lower the correlation):", quote=FALSE)
print(v)
writeLines("\n")

# Load the package ggplot2. 
library(ggplot2)

# D. Plot frequency bar plots of amCoded and vsCoded.
# (1) Convert the table tbl to the data frame df_tbl.
df_tbl <- as.data.frame(tbl)
# The content of df_tbl is:
#      Var1      Var2 Freq
# 1 v-shaped automatic   12
# 2 straight automatic    7
# 3 v-shaped    manual    6
# 4 straight    manual    7

# (2) Rename the columns of the data frame df_tbl to vsCoded, amCoded and Freq.
names(df_tbl) <- c("vsCoded", "amCoded", "Freq")
# The content of df_tbl is now:
#   vsCoded   amCoded NA
#1 v-shaped automatic 12
#2 straight automatic  7
#3 v-shaped    manual  6
#4 straight    manual  7

# (3) Bar plot with counts. 
dev.new()   # Create a new window to plot in.
plot(ggplot(df_tbl, aes(fill=df_tbl$amCoded, y=df_tbl$Freq, x=df_tbl$vsCoded)) + 
    geom_bar(position="dodge", stat="identity") + scale_fill_brewer(palette = "Set2")) 

# (4) Bar plot with percentages.
dev.new()
plot(ggplot(df_tbl, aes(fill=df_tbl$amCoded, y=df_tbl$Freq, x=df_tbl$vsCoded)) + 
    geom_bar( stat="identity", position="fill") + scale_fill_brewer(palette = "Set2")) 

