# comments 
# still buggy when a file/folder already exists

require(data.table)
require(tidyr)
library(plyr)
library(dplyr)
library(readr)
library(stringr)
library(gmodels)


 # dir = "Homog_monthly_mean_temp"
 # temp_meas = list.files(path=dir, pattern="*.txt", full.names=TRUE)
 # i = 286;


clean_data <- function(temp_meas, dir)
  for (i in 1:length(temp_meas)){
    title = read.table(temp_meas[i], nrow = 1, sep = ",",dec = ".", as.is = TRUE,quote = "\"", na.strings=c(" "), header = FALSE)
    stationNum_city_prov = sprintf("%s_%s_%s", title[1],trimws(title[2]),strtrim(title[3],2))
    assign(stationNum_city_prov, read.delim(temp_meas[i],skip = 2, header = FALSE))
    (stationNum_city_prov)
    hdr = read.table(temp_meas[i], skip = 2, nrow = 1, sep = ",", as.is = TRUE, na.strings=c(" "))
    hdr <- hdr[, colSums(is.na(hdr)) == 0]
    (hdr)
    dat = read.delim(temp_meas[i], skip = 4, header= FALSE, as.is=TRUE, dec = ",", sep = ",", na.strings=c(" "))
    dat <- dat[ ,colSums(is.na(dat)) == 0]

    dat <- data.frame(lapply(dat, function(x){
      gsub("[a-zA-Z]", NA, x)
    }))
    #tempDat = dat
     dat <- dat[,colSums(is.na(dat))==0 ]
    
    #filter out -9999.9 - default values 
    dat <- data.frame(lapply(dat, function(x){
      gsub("-9999.9", "NA", x)
    }))
    # dat <- dat[rowSums(is.na(dat))==0, ]
    
    total = rbind(hdr, dat)
    parentPath = "C:/Environment_Canada_Shiny_App/Shiny_App/"
    filePath= sprintf("%s%s_cleaned/%s.txt", parentPath,dir,stationNum_city_prov)
    write.table(total, filePath, append = FALSE, sep = " ", dec = ".",
                row.names = FALSE, col.names = FALSE)
   }



minTempDir = "Homog_monthly_min_temp"
maxTempDir = "Homog_monthly_max_temp"
meanTempDir = "Homog_monthly_mean_temp" 


#temp_meas <- array(c(minTempDir,maxTempDir,meanTempDir))
# tempMax = list.files(path=maxTempDir, pattern="*.txt", full.names=TRUE)
# clean_data(tempMax,maxTempDir)
tempMin  = list.files(path=minTempDir, pattern="*.txt", full.names=TRUE)
clean_data(tempMin,minTempDir)
# tempMean = list.files(path=meanTempDir, pattern="*.txt", full.names=TRUE)
# clean_data(tempMean,meanTempDir)

# Create list of text files
txt_files_ls = list.files(path="Homog_monthly_min_temp_cleaned", pattern="*.txt", full.names = TRUE)
names = list.files(path="Homog_monthly_min_temp_cleaned", pattern="*.txt")
ns = matrix(unlist(strsplit(names,'_')),ncol = 3,byrow = TRUE)

provs <- c("AB","BC","YT","NT","NU","SK", "MB", "ON", "QC", "NB", "NS", "PE", "NL")
list_prov = list()
names_city = list()
j = 1
for (i in 1:length(txt_files_ls))
  if(ns[i,3] == "YT.txt"){
    list_prov[j] <- txt_files_ls[i]
    names_city[j] <- ns[i,2]
    j = j+1
  }


txt_files_df <- lapply(list_prov, function(x) {read.table(file = x, header = T, sep = " ",dec = ".", colClasses = "factor")})

# Combine them
combined_df <- do.call("rbind", lapply(txt_files_df, as.data.frame)) 


# 
# # Peak data to make sure there are no NA values 
# # %between%
# years_1980_greater<-combined_df[combined_df$X.Year>1979,]
# # years_1980_greater = setDT(combined_df)[X.Year > 1979]
# (is.na(years_1980_greater))
# 
# x_year <- as.numeric(as.character(unlist(years_1980_greater$X.Year)))
# y_temp <- as.numeric(as.character(unlist(years_1980_greater$X.....Feb)))
# 
# # data_1980_greater_Feb <- data.frame(cbind(x_year,y_temp))
# 
# fit_3  <- lm(y_temp ~ x_year, data = data_1980_greater_Feb)
# fit_3
# summary(fit_3)
# anova(fit_3)
# plot(y_temp~x_year, data = data_1980_greater_Feb, main ="Min Temperatures - February - AB", xlab = "year", ylab = "temp")
# abline(fit_3, col = "red")

city <- data.frame(names_city)
city <- as.matrix(city)
city <- matrix(city, ncol = ncol(city), dimnames = NULL)
city <- t(city)
city <- factor(city)
city <- as.matrix(city)
city <- as.character(city)
city <- factor(city)

# fit_1 <- list()
# R_2 <- list()
# b <- list()
# CIs <-list()
# y_city <- list()
# x_year <- data.frame(x_year)
# 
# for (i in 1:length(names_city)){
#   index <- which(input_df[, "city"] == unlist(names_city)[i])
#   fit_1 <- lm(y_city_year[index]~x_year[index], data = input_df )
#   b[[i]] <- fit_1$coefficients
#   R_2[i] <- as.numeric(unlist(summary(fit_1)$r.squared))
#   CIs[[i]] <- ci(fit_1, 0.95, alpha=1-0.95, na.rm = TRUE)
#   # x_city <- cbind(1,test_x_year[[i]])
#   # y_city[[i]] <- x_city%*%as.matrix(b[[i]])
# }


# Interaction Model - Regression Results 


years_1980_greater<-combined_df[as.numeric(as.character(combined_df$X.Year))>1979,]
x_year <- as.numeric(as.character(unlist(years_1980_greater$X.Year)))
y_city_year <- as.numeric(as.character(unlist(years_1980_greater$X.....Feb)))


id <- list()
num_na <- c()
curr_length <- c()

for(i in 1: length(names_city)){
  # index <- which(input_df[, "city"] == unlist(names_city)[i])
  curr_df <- txt_files_df[[i]]
  curr_df_1980<-curr_df[as.numeric(as.character(curr_df$X.Year))>1979,]
  y_curr_df <- as.numeric(as.character(unlist(curr_df_1980$X.....Feb)))
  num_na[i] <- sum(is.na(y_curr_df))
  curr_length[i] <- length(y_curr_df)
  id[i] <- list(rep.int(i, curr_length[i]))
}

df_n <- curr_length - num_na 
df_n <- data.frame(df_n)
city<- data.table(city, stringsAsFactors = TRUE)
id<- as.factor(unlist(id))
city$id <- rownames(city) 
city$prov <- "BC"
df_n<- data.frame(df_n, city[,1])

input_df <- data.table(y_city_year, x_year, id)
input_df <- merge(x = input_df, y = city, by = 'id', all = TRUE)

fit_2 <- lm(y_city_year~ city-1 + city*x_year , data = input_df)



b_2 <- fit_2$coefficients
r.squared <- summary(fit_2)$r.squared
critical_value <- qt(0.95, (df_n[,1]-2))
standard_error <- summary(fit_2)$coef[,2]
standard_error_slope <- standard_error[(length(names_city)+1):length(standard_error)]
margin_error <- critical_value*standard_error_slope
estimate <- summary(fit_2)$coef[,1]
estimate_slope <- estimate[(length(names_city)+1):length(estimate)]
CI_lower <-  estimate_slope - margin_error
CI_upper <- estimate_slope + margin_error
CI <- data.frame(CI_lower, CI_upper) 
city_matrix <- city[order(city), ] 
intercept <- data.frame("intercept"=b_2[1:length(names_city)])
slope <- data.frame("slope" = b_2[(length(names_city)+1):length(b_2)])
variance <- (summary(fit_2)$coef[,2])^2
variance_slope <- data.frame("variance" = variance[(length(names_city)+1):length(variance)])
# split <- matrix(unlist(strsplit(rownames(check),'city')),ncol = 2,byrow = TRUE)
r_1 <- data.frame(city_matrix, intercept, slope, CI_lower, CI_upper, variance_slope, r.squared)
r_2 <- merge(x = df_n, y = r_1, by= 'city' , all  = TRUE)
# r_3 <- r_2[,c(1,3,4,7,8,2,5)]



hist(r_2$slope, freq = TRUE, main  = "Histogram of Slope - YT", xlab = "Slope")
hist(r_2$slope, prob = TRUE, main  = "Histogram of Slope - YT", xlab = "Slope")
lines(density(r_2$slope), col = "red")

# hist(x = CI_lower_slope, prob = TRUE)
# lines(density(CI_lower_slope), col = "red")

write.csv(r_2,'reg_results_YT.csv')



