
data_dir = "C:/Users/SylvieOng/Desktop/TempJanice/DataJobApp/r_ss_analysis/"
data_file = "LFS-71M0001-E-2019-June_F1.csv"
data_path = paste(data_dir, data_file, sep = '')
writeLines("\n")
print("Reading data from file:", quote=FALSE)
print(data_path)
writeLines("\n")

df_all <- read.table(data_path, header=TRUE, sep=",", quote="\"") # read csv file into a dataframe
print("Number of rows and columns of input table:", quote=FALSE) 
print(dim(df_all))
writeLines("\n")
View(df_all)    # spreadsheet view

# map a bunch of numerically coded variables to categorical
# UNION is currently coded as numerical, map it to categorical
df_all$UNIONCoded = cut(df_all$UNION, breaks = c(0, 1, 2, 3), labels = c("Union member", "Non-member", "Non-unionized"))

# PROV is currently coded as numerical, map it to categorical
df_all$PROVCoded = cut(df_all$PROV, breaks = c(0, 10, 11, 12, 13, 24, 35, 46, 47, 48, 59), 
    labels = c("NL", "PE", "NS", "NB", "QC", "ON", "MB", "SK", "AB", "BC" ))

# chi test: UNION, PROV
tbl = table(df_all$UNIONCoded, df_all$PROVCoded) 

chi2 = chisq.test(tbl, correct=F)
p = chi2$p.value
v = sqrt(as.numeric(chi2$statistic) / sum(tbl))

# display the table and stats
print(tbl)

print("p-value:", quote=FALSE)
print(p)

print("Cramer's V (the smaller v, the lower the correlation):", quote=FALSE)
print(v)
writeLines("\n")

library(ggplot2)

# bar plots - x: PROV, by: UNION
df = as.data.frame(tbl)

# counts
dev.new()
plot(ggplot(df, aes(fill=df$Var1, y=df$Freq, x=df$Var2)) + 
    geom_bar(position="dodge", stat="identity") + scale_fill_brewer(palette = "Set2")) 

# percentages
dev.new()
plot(ggplot(df, aes(fill=df$Var1, y=df$Freq, x=df$Var2)) + 
    geom_bar( stat="identity", position="fill") + scale_fill_brewer(palette = "Set2")) 

