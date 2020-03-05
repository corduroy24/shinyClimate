require(data.table)
require(tidyr)
library(plyr)
library(dplyr)
library(readr)
library(stringr)
library(gmodels)

# comments
# still buggy when a file/folder already exists
#reproduce code, -> software engineering


# dir = "Homog_monthly_min_temp"
# temp_meas = list.files(path=dir, pattern="*.txt", full.names=TRUE)
# i = 1;

# main function later... 
# handling new data ?
#
minTempDir = "Homog_monthly_min_temp"
maxTempDir = "Homog_monthly_max_temp"
meanTempDir = "Homog_monthly_mean_temp"


# tempMax = list.files(path=maxTempDir, pattern="*.txt", full.names=TRUE)
# clean_data(tempMax,maxTempDir)
# tempMin  = list.files(path=minTempDir, pattern="*.txt", full.names=TRUE)
# clean_data(tempMin,minTempDir)
# tempMean = list.files(path=meanTempDir, pattern="*.txt", full.names=TRUE)
# clean_data(tempMean,meanTempDir)


all_provs <- data.frame("provs" = c("AB","BC","YT","NT","NU","SK", "MB", "ON", "QC", "NB", "NS", "PE", "NL"))
# single_prov <- data.frame("provs" = c('AB'))
# provs <-data.frame("provs" = c("AB", "BC")) #convert to preset regions...

provs <- all_provs
year_to_start <- 1980
month <- 'Feb'
temp_meas <- 'min_temp'

if(file.exists(paste(temp_meas, month, year_to_start))){
  file.exists(paste(temp_meas,month, year_to_start,'.RData'))
  load(paste(temp_meas,month, year_to_start,'.RData'))
} else {
  combined_df <-data.frame()
  for(i in 1:nrow(provs)){
    input_df <- load_cleaned_data(year_to_start, month, temp_meas, provs[i,]) #data matrix X
    output_df <- regression(input_df) #reg results 
    # make_boxplot(output_df)
    combined_df <- rbind(combined_df, output_df)
  }
  save(combined_df, file = paste(temp_meas, month, year_to_start,'.RData'))
}


provs <- unique(combined_df[, 'prov'])
for (i in 1:length(provs)){
  index <- which(combined_df[, "prov"] == provs[i])
  output_df <- combined_df[index,]
  make_hist(output_df)
}
  
clean_data <- function(temp_meas, dir)
  for (i in 1:length(temp_meas)){
    title = read.table(temp_meas[i], nrow = 1, sep = ",",dec = ".", as.is = TRUE,quote = "\"", na.strings=c(" "), header = FALSE)
    stationNum_city_prov = sprintf("%s_%s_%s", title[1],trimws(title[2]),strtrim(title[3],2))
    assign(stationNum_city_prov, read.delim(temp_meas[i],skip = 2, header = FALSE))
    (stationNum_city_prov)
    hdr = read.table(temp_meas[i], skip = 2, nrow = 1, sep = ",", as.is = TRUE, na.strings=c(" "), strip.white = TRUE)
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

# Load data from cleaning step

load_cleaned_data <- function(year, month, temp_meas, nom_prov){
  if(temp_meas == 'min_temp'){
    txt_files_ls = list.files(path="Homog_monthly_min_temp_cleaned", pattern="*.txt", full.names = TRUE)
    names = list.files(path="Homog_monthly_min_temp_cleaned", pattern="*.txt")
  }
  else if(temp_meas == 'max_temp'){
    txt_files_ls = list.files(path="Homog_monthly_max_temp_cleaned", pattern="*.txt", full.names = TRUE)
    names = list.files(path="Homog_monthly_max_temp_cleaned", pattern="*.txt")
  }
  else if(temp_meas == 'mean_temp'){
    txt_files_ls = list.files(path="Homog_monthly_mean_temp_cleaned", pattern="*.txt", full.names = TRUE)
    names = list.files(path="Homog_monthly_mean_temp_cleaned", pattern="*.txt")
  }
  
  ns = matrix(unlist(strsplit(names,'_',)),ncol = 3,byrow = TRUE)
  input_df <- data.frame()
  for (i in 1:length(txt_files_ls)){
    if(unlist(strsplit(ns[i,3],'.txt')) == nom_prov){
      nom_city <- ns[i,2]
      nom_prov <- unlist(strsplit(ns[i,3],'.txt'))
      # Non-breaking spaces...trim.white doesnt work... 
      txt_files_df <- read.table(file = txt_files_ls[i], header = TRUE, sep = " ",dec = ".", colClasses = "factor")
      years_greater<-txt_files_df[as.numeric(as.character(txt_files_df$Year))>=year_to_start,]
      y_temp <- suppressWarnings(as.numeric(as.character(unlist(years_greater[,month]))))
      x_year <- suppressWarnings(as.numeric(as.character(unlist(years_greater[,'Year']))))
      temp_df <- data.frame(y_temp, x_year, "city" = nom_city, "prov" = nom_prov) 
      input_df <- rbind(input_df, temp_df) 
    }
  }
  return(input_df)
}


regression <- function(input_df){
  city_vector <- unique(input_df[,"city"])
  prov <- unique(input_df[, 'prov'])
  output_df <- data.frame()
  for (i in 1:length(city_vector)){
    index <- which(input_df[, "city"] == city_vector[i])
    fit <- lm(y_temp[index]~x_year[index], data = input_df)
    b <- data.frame("intercept" = fit$coefficients[1], "slope" = fit$coefficients[2])
    R_2 <- data.frame("r.squared" = as.numeric(unlist(summary(fit)$r.squared)))
    # CIs <- ci(fit, 0.95, alpha=1-0.95, na.rm = TRUE)
    critical_value <- qt((1-0.95)/2, (nrow(fit$model)-1))
    standard_error <- summary(fit)$coef[,2][2]
    margin_error <- critical_value*standard_error
    estimate <- summary(fit)$coef[,1][2]
    CI_lower <-  estimate - margin_error
    CI_upper <- estimate + margin_error
    variance <- (standard_error)^2
    # prov <- unique(as.character(input_df[, 'prov'][index]))
    curr_results_df <- data.frame("city"=city_vector[i],'prov' = prov,  b,"r.squared"=R_2,CI_lower, CI_upper,variance,"n"=nrow(fit$model), row.names = NULL)
    output_df <- rbind(output_df,curr_results_df) 
  }
  return(output_df)
}

# Interaction Model - Confirm Regression Results
# 
# city<- data.table(city_vector, stringsAsFactors = TRUE)
# fit_2 <- lm(y_temp~ city-1 + city*x_year , data = input_df)

make_hist<- function(output_df){
  hist <-hist(output_df$slope, freq = TRUE, main  = paste("Histogram of Slope - ",unique(output_df[,"prov"])), xlab = "Slope")
  save(hist, file = paste(temp_meas, month, year_to_start,'.RData'))
}

#
# hist(r_2$slope, prob = TRUE, main  = "Histogram of Slope - NL", xlab = "Slope")
# lines(density(r_2$slope), col = "red")
#
# # hist(x = CI_lower_slope, prob = TRUE=)
# # lines(density(CI_lower_slope), col = "red")
#
# write.csv(r_2,'reg_results_NL.csv')

# boxplot(reg_results_AB$slope~reg_results_AB$prov)
  



# boxplot(combined_reg_results$slope~combined_reg_results$prov)

# missing - CI plots, r.squared?
