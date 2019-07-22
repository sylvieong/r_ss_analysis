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
View(df_all)    # spreadsheet view

# map a bunch of numerically coded variables to categorical
# Gender is currently coded as numerical (0 and 1), map it to categorical
df_all$GenderCoded = cut(df_all$Gender, breaks = c(-1, 0, 1), labels = c("Male", "Female"))

# Athete is currently coded as numerical (0 and 1), map it to categorical
df_all$AthleteCoded = cut(df_all$Athlete, breaks = c(-1, 0, 1), labels = c("Athlete_N", "Athlete_Y"))

# Smoking is currently coded as numerical (0 and 1), map it to categorical
df_all$SmokingCoded = cut(df_all$Smoking, breaks = c(-1, 0, 1), labels = c("Smoking_N", "Smoking_Y"))

# LiveOnCampus is currently coded as numerical (0 and 1), map it to categorical
df_all$LiveOnCampusCoded = cut(df_all$LiveOnCampus, breaks = c(-1, 0, 1), labels = c("LiveOnCampus_N", "LiveOnCampus_Y"))

# do chi-test pairwise interaction of a bunch of categorical variables: 
# GenderCoded, AthleteCoded, SmokingCoded, LiveOnCampusCoded, State
# Question: which is the most correlated with being an Athlete


# create a function to perform chitest and print out results
chi_test_results <- function (tbl) {
    # check that object input is actually a table    
    if (class(tbl) != "table") stop("Not an object of class 'table' ")

    # compute all the stats we need
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


    results <- list("p" = p, "v" = v)
    return(results)
}


check_best <- function (res) {
    # check that object input is actually a list    
    if (class(res) != "list") stop("Not an object of class 'list' ")


    if (res$p < best_p) {
      assign("best_p", res$p, envir = .GlobalEnv)
      assign("best_p_var", res$var, envir = .GlobalEnv)
    } 

    if (res$v > best_v) {
      assign("best_v", res$v, envir = .GlobalEnv)
      assign("best_v_var", res$var, envir = .GlobalEnv)
    } 
}
 

best_p = 1
best_v = 0
best_p_var = ""
best_v_var = ""

# gender
curr_tbl = table(df_all$GenderCoded, df_all$AthleteCoded) 
curr_res = chi_test_results(curr_tbl)
curr_res$var = "Gender"
check_best(curr_res)


# smoking
curr_tbl = table(df_all$SmokingCoded, df_all$AthleteCoded) 
curr_res = chi_test_results(curr_tbl)
curr_res$var = "Smoking"
check_best(curr_res)


# living on campus
curr_tbl = table(df_all$LiveOnCampusCoded, df_all$AthleteCoded) 
curr_res = chi_test_results(curr_tbl)
curr_res$var = "LiveOnCampus"
check_best(curr_res)


# in or out of state
curr_tbl = table(df_all$State, df_all$AthleteCoded) 
curr_res = chi_test_results(curr_tbl)
curr_res$var = "State"
check_best(curr_res)

print("Best p value:", quote=FALSE)
print(best_p_var, quote=FALSE)
print(best_p)

print("Best v value:", quote=FALSE)
print(best_v_var, quote=FALSE)
print(best_v)

# plots with Athlete, Gender, Smoking

# bar plots - x: Athete, by: Gender
tbl = table(df_all$GenderCoded, df_all$AthleteCoded) 
df = as.data.frame(tbl)

# counts
dev.new()
plot(ggplot(df, aes(fill=df$Var1, y=df$Freq, x=df$Var2)) + 
    geom_bar(position="dodge", stat="identity") + scale_fill_brewer(palette = "Set2")) 

# percentages
dev.new()
plot(ggplot(df, aes(fill=df$Var1, y=df$Freq, x=df$Var2)) + 
    geom_bar( stat="identity", position="fill") + scale_fill_brewer(palette = "Set2")) 

# bar plots - x: Athete, by: Smoking
tbl = table(df_all$SmokingCoded, df_all$AthleteCoded) 
df = as.data.frame(tbl)

# counts
dev.new()
plot(ggplot(df, aes(fill=df$Var1, y=df$Freq, x=df$Var2)) + 
    geom_bar(position="dodge", stat="identity") + scale_fill_brewer(palette = "Set2")) 

# percentages
dev.new()
plot(ggplot(df, aes(fill=df$Var1, y=df$Freq, x=df$Var2)) + 
    geom_bar( stat="identity", position="fill") + scale_fill_brewer(palette = "Set2")) 


# interaction among three variables
tbl = table(df_all$GenderCoded, df_all$SmokingCoded, df_all$AthleteCoded)
df = as.data.frame(tbl)

library(vcd)
tab = xtabs(Freq ~ Var1+Var2+Var3, data=df)
dev.new()
mosaic(data=tab, ~ Var1+Var2+Var3, shade=TRUE, cex=2.5)



