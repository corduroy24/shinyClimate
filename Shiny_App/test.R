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
    parentPath = "C:/Shiny_App/"
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

list_prov = list()
names_city = list()
j = 1
for (i in 1:length(txt_files_ls))
  if(ns[i,3] == "AB.txt"){
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

idk_test <- list()
fit <- list()
test_y_temp <- list()
test_x_year <- list()
fit_1 <- list()
R_2 <- list()
b <- list()
CIs <-list()
y_city <- list()

# for (i in 1:length(names_city)){
#   temp_file <- txt_files_df[[i]]
#   test_data_1980_greater<-temp_file[temp_file$X.Year > 1979,]
#   idk_test[[i]] <- test_data_1980_greater
#   test_y_temp[[i]] <- as.numeric(as.character(unlist(idk_test[[i]]$X.....Feb)))
#   test_x_year[[i]] <- as.numeric(as.character(unlist(idk_test[[i]]$X.Year)))
#   fit_1 <- lm(test_y_temp[[i]]~test_x_year[[i]])
#   b[[i]] <- fit_1$coefficients
#   R_2[i] <- as.numeric(unlist(summary(fit_1)$r.squared))
#   CIs[[i]] <- ci(fit_1, 0.95, alpha=1-0.95, na.rm = TRUE)
#   x_city <- cbind(1,test_x_year[[i]])
#   y_city[[i]] <- x_city%*%as.matrix(b[[i]])
# }



id <- list()
# %between%
years_1980_greater<-combined_df[as.numeric(as.character(combined_df$X.Year))>1979,]
x_year <- as.numeric(as.character(unlist(years_1980_greater$X.Year)))
y_city_year <- as.numeric(as.character(unlist(years_1980_greater$X.....Feb)))
num_na <- c()
curr_length <- c()
for(i in 1: length(names_city)){
  curr_city_df <- txt_files_df[[i]]
  curr_city_1980_greater_df <- curr_city_df[as.numeric(as.character(curr_city_df$X.Year))>1979, ]
  curr_temp_feb_df <- as.numeric(as.character(unlist(curr_city_1980_greater_df$X.....Feb)))
  num_na[i] <- sum(is.na(curr_temp_feb_df ))
  curr_length[i] <- nrow(curr_city_1980_greater_df)
  id[i] <- list(rep.int(i, curr_length[i]))
}

df_n <- curr_length - num_na 
df_n <- data.frame(df_n)
df_n$city <- city[,1]
df_n$id <- city[,2]
id<- as.factor(unlist(id))
before_df <- data.table(y_city_year, x_year, id)
city<- data.table(city, stringsAsFactors = TRUE)
city$id <- rownames(city) 
city$prov <- "AB"
test <- data.frame(y_city_year)

before_df <- merge(x = before_df, y = city, by = 'id', all = TRUE)
fit_2 <- lm(y_city_year~ city-1 + city*x_year , data = before_df)
b_2 <- fit_2$coefficients
R_2_2 <- list()

for(i in 1: length(names_city))
  R_2_2[i] <- as.numeric(unlist(summary(fit_2)$r.squared))


CI_lower <- summary(fit_2)$coef[,1] - summary(fit_2)$coef[,2]
CI_upper <- summary(fit_2)$coef[,1] + summary(fit_2)$coef[,2]
CI_lower_slope  <- CI_lower[(length(names_city)+1):length(CI_lower)]
CI_upper_slope <- CI_upper[(length(names_city)+1):length(CI_upper)]

city_matrix <- city[order(city), ] 
intercept <- b_2[1:length(names_city)]
slope <- b_2[(length(names_city)+1):length(b_2)]
check <- data.frame(intercept)
check_3 <- data.frame(slope)
split <- matrix(unlist(strsplit(rownames(check),'city')),ncol = 2,byrow = TRUE)
check$city <- split[,2]
check_3$city <- split[,2]
cc <- merge(x = check, y = check_3, by = 'city', all = TRUE)
check_2 <- merge(x = cc, y = city_matrix, by = 'city' , all  = TRUE)
check_4 <- merge(x = check_2, y = df_n, by = 'id' , all  = TRUE)
results <- data.frame(city_matrix, intercept, slope, df_n)

results <- results[,c(1,4,5,3,2)]
provs <- c("BC","YT","NT","NU", "AB","SK", "MB", "ON", "QC", "NB", "NS", "PE", "NL")






