library(data.table)
library(plyr)
library(tidyverse)
library(tidyr)
library(gmodels)
library(log4r)

logger <- create.logger()
logfile(logger) <- 'debug.log'
level(logger) <- 'DEBUG'


# Comments/To-do 
# still buggy when a file/folder already exists
# reproducable code, -> software engineering
# Histograms of CIs
# notebook for data  cleaning - next week - marchs 11
# review shiny app - mid march 
# main function later... 
# handling user adding new data ? - clean function needs to be better...,
# in terms of the folder, or how its saved ???

# dir = "Homog_monthly_min_temp"
# temp_val = list.files(path=dir, pattern="*.txt", full.names=TRUE)
# i = 1;


minTempDir = "Homog_monthly_min_temp"
maxTempDir = "Homog_monthly_max_temp"
meanTempDir = "Homog_monthly_mean_temp"


# tempMax = list.files(path=maxTempDir, pattern="*.txt", full.names=TRUE)
# clean_data(tempMax,maxTempDir)
# tempMin  = list.files(path=minTempDir, pattern="*.txt", full.names=TRUE)
# clean_data(tempMin,minTempDir)
# tempMean = list.files(path=meanTempDir, pattern="*.txt", full.names=TRUE)
# clean_data(tempMean,meanTempDir)

#params - decorator ???
# provs <- all_provs
# year_to_start <- 1980
# month <- 'Feb'
# temp_val <- 'min_temp'

#create function here 
main <- function(temp_val, month, year_to_start){
  provs <- data.frame("provs" = c("AB","BC","YT","NT","NU","SK", "MB", "ON", "QC", "NB", "NS", "PE", "NL"))
  if(file.exists(paste(temp_val,month, year_to_start,'.RData'))){
    load(paste(temp_val,month, year_to_start,'.RData'))
    p("Rdata exists")
  } else {
    p("RData does not exists")
    input_df_all <-data.frame()
    output_df_all <-data.frame()
    for(i in 1:nrow(provs)){
      input_df <- load_cleaned_data(year_to_start, month, temp_val, provs[i,]) #data matrix X
      output_df <- regression(input_df) #reg results 
      input_df_all <- rbind(input_df_all, input_df)
      output_df_all <- rbind(output_df_all, output_df)
    }
    # save(input_df_all, file = paste(temp_val, month, year_to_start,'.RData'))
    # save(output_df_all, file = paste(temp_val, month, year_to_start,'.RData'))
  }
  
  return(output_df_all)
}

# add_data <- function(temp_val, old_df, new_data){
#   for (i in 1:length(temp_val)){
#     
#     # title = read.table(temp_val[i], nrow = 1, sep = ",",dec = ".", as.is = TRUE,quote = "\"", na.strings=c(" "), header = FALSE)
#     # stationNum_city_prov = sprintf("%s_%s_%s", title[1],trimws(title[2]),strtrim(title[3],2))
#     # assign(stationNum_city_prov, read.delim(temp_val[i],skip = 2, header = FALSE))
#     # (stationNum_city_prov)
#     # hdr = read.table(temp_val[i], skip = 2, nrow = 1, sep = ",", as.is = TRUE, na.strings=c(" "), strip.white = TRUE)
#     # hdr <- hdr[, colSums(is.na(hdr)) == 0]
#     # (hdr)
#     dat = read.delim(temp_val[i], skip = 4, header= FALSE, as.is=TRUE, dec = ",", sep = ",", na.strings=c(" "))
#     dat <- dat[ ,colSums(is.na(dat)) == 0]
#     
#     #clean random data... 
#     dat <- data.frame(lapply(dat, function(x){
#       gsub("[a-zA-Z]", NA, x)
#     }))
#     dat <- dat[,colSums(is.na(dat))==0 ]
#     
#     #filter out -9999.9 - default values
#     dat <- data.frame(lapply(dat, function(x){
#       gsub("-9999.9", "NA", x)
#     }))
# 
#     total = rbind(old_df, new_df)
#   }
# } 

clean_data <- function(temp_val, dir)
  for (i in 1:length(temp_val)){
    title = read.table(temp_val[i], nrow = 1, sep = ",",dec = ".", as.is = TRUE,quote = "\"", na.strings=c(" "), header = FALSE)
    stationNum_city_prov = sprintf("%s_%s_%s", title[1],trimws(title[2]),strtrim(title[3],2))
    assign(stationNum_city_prov, read.delim(temp_val[i],skip = 2, header = FALSE))
    (stationNum_city_prov)
    hdr = read.table(temp_val[i], skip = 2, nrow = 1, sep = ",", as.is = TRUE, na.strings=c(" "), strip.white = TRUE)
    hdr <- hdr[, colSums(is.na(hdr)) == 0]
    (hdr)
    dat = read.delim(temp_val[i], skip = 4, header= FALSE, as.is=TRUE, dec = ",", sep = ",", na.strings=c(" "))
    dat <- dat[ ,colSums(is.na(dat)) == 0]
    
    #cleaning step ........
    #clean random data... 
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

find_files <- function(temp_val){
  if(temp_val == 'min_temp'){
    txt_files_ls = list.files(path="Homog_monthly_min_temp_cleaned", pattern="*.txt", full.names = TRUE)
    names = list.files(path="Homog_monthly_min_temp_cleaned", pattern="*.txt")
  }
  else if(temp_val == 'max_temp'){
    txt_files_ls = list.files(path="Homog_monthly_max_temp_cleaned", pattern="*.txt", full.names = TRUE)
    names = list.files(path="Homog_monthly_max_temp_cleaned", pattern="*.txt")
  }
  else if(temp_val == 'mean_temp'){
    txt_files_ls = list.files(path="Homog_monthly_mean_temp_cleaned", pattern="*.txt", full.names = TRUE)
    names = list.files(path="Homog_monthly_mean_temp_cleaned", pattern="*.txt")
  }
  
  temp_object <- list(txt_files_ls, names) 
  
  return(temp_object)
}
# Load data from cleaning step
# dont have to sort it by province... 
load_cleaned_data <- function(year_to_start, month, temp_val, nom_prov){
  temp_object <- find_files(temp_val)
  txt_files_ls <- temp_object[[1]]
  names <- temp_object[[2]]
  
  ns = matrix(unlist(strsplit(names,'_',)),ncol = 3,byrow = TRUE)
  
  #build data frame. 
  
  input_df <- data.frame()
  # for later... 
  # all <- unlist(strsplit(ns[,3],'.txt'))
  # index <- which(all == nom_prov)
  # debug(logger, paste('| index |',index,"|"))
  
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

check_start_year_cutoff <- function(temp_val){
  temp_object <- find_files(temp_val)
  txt_files_ls <- temp_object[[1]]
  names <- temp_object[[2]]
  most_recent_year <-c()
  for (i in 1:length(txt_files_ls)){
    txt_files_df <- read.table(file = txt_files_ls[i], header = TRUE, sep = " ",dec = ".", colClasses = "factor")
    x_temp <- suppressWarnings(as.numeric(as.character(unlist(txt_files_df[,'Year']))))
    most_recent_year[i] <- max(x_temp)
  }
  most_recent_year <- most_recent_year[!is.na(most_recent_year)]
  # debug(logger, paste('|most_recent_year ' , '|', most_recent_year,"|"))
  start_year_cutoff <- min(most_recent_year, na.rm = TRUE)
  # debug(logger, paste('|min ' , '|',start_year_cutoff,"|"))
  return(start_year_cutoff)
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
  # plot(y_temp~x_year, data = input_df)
  # abline(fit, col = 'red')
  return(output_df)
}

# Interaction Model - Confirm Regression Results
# 
# city<- data.table(city_vector, stringsAsFactors = TRUE)
# fit_2 <- lm(y_temp~ city-1 + city*x_year , data = input_df)


overlay_slopes <- function(city, prov){
  
  
}

# hist(output_df_all$slope, freq = TRUE, main  = paste("Histogram of Slope(Canada)"), xlab = "Slope")
# abline(h=0, col = 'red')
# boxplot(output_df_all$r.squared~output_df_all$prov, xlab = "Province", ylab= 'r.squared', main = 'Boxplot of R_2')
# boxplot(output_df_all$slope~output_df_all$prov, xlab = 'Province',ylab = 'Slope', main = 'Boxplot of slope')


# hist_slopes <- function(output_df_all){
#   provs <- unique(output_df_all[, 'prov'])
#   for (i in 1:length(provs)){
#     index <- which(output_df_all[, "prov"] == provs[i])
#     output_df <- output_df_all[index,]
#     hists <- hist(output_df$slope, freq = TRUE, main  = paste("Histogram of Slope - ",unique(output_df[,"prov"])), xlab = "Slope")
#     #histogram for CIs
#   }
#   return(hists)
# }