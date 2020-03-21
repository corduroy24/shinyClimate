library(data.table)
library(plyr)
library(tidyverse)
library(tidyr)
library(gmodels)
library(log4r)

logger <- create.logger()
city_vector <- data.frame()
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

year_to_start <- 1980
month <- 'Feb'
temp_val <- 'min_temp'
# check <- data.frame()
#create function here 
main <- function(temp_val, month, year_to_start){
  provs <- data.frame("provs" = c("AB","BC","YT","NT","NU","SK", "MB", "ON", "QC", "NB", "NS", "PE", "NL"))
  if(file.exists(paste('RData/',temp_val,month, year_to_start,'.RData'))){
    load(paste('RData/',temp_val,month, year_to_start,'.RData'), .GlobalEnv)
    debug(logger, paste("Rdata exists"))
  } else {
    debug(logger, paste("RData does not exists"))
    input_df_all <- load_cleaned_data(year_to_start, month, temp_val) #data matrix X
    output_df_all <- regression(input_df_all) #reg results 
    save(input_df_all, output_df_all, file = paste('RData/',temp_val, month, year_to_start,'.RData'))
    load(paste('RData/',temp_val,month, year_to_start,'.RData'), .GlobalEnv)
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
load_cleaned_data <- function(year_to_start, month, temp_val){
  temp_object <- find_files(temp_val)
  txt_files_ls <- temp_object[[1]]
  names <- temp_object[[2]]
  
  ns = matrix(unlist(strsplit(names,'_',)),ncol = 3,byrow = TRUE)
  
  # if i wanted to do by province still... 
  # all <- unlist(strsplit(ns[,3],'.txt'))
  # index <- which(all == nom_prov)
  # debug(logger, paste('| index |',index,"|"))
  
  # build input data frame. 
  input_df <- data.frame()
  for (i in 1:length(txt_files_ls)){
      nom_city <- ns[i,2]
      nom_prov <- unlist(strsplit(ns[i,3],'.txt'))
      # debug(logger, paste('| province name |',nom_prov,"|"))
      
      # Non-breaking spaces...trim.white doesnt work... 
      txt_files_df <- read.table(file = txt_files_ls[i], header = TRUE, sep = " ",dec = ".", colClasses = "factor")
      years_greater<-txt_files_df[as.numeric(as.character(txt_files_df$Year))>=year_to_start,]
      y_temp <- suppressWarnings(as.numeric(as.character(unlist(years_greater[,month]))))
      x_year <- suppressWarnings(as.numeric(as.character(unlist(years_greater[,'Year']))))
     
      temp_df <- data.frame(y_temp, x_year, "city" = nom_city, "prov" = nom_prov) 

      input_df <- rbind(input_df, temp_df) 
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
  city_prov_vector <- unique(input_df[,c("city", 'prov')])
  city_vector <- city_prov_vector[, 'city']
  prov_vector <- city_prov_vector[, 'prov']
  
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
    
    curr_results_df <- data.frame("city"=city_vector[i],'prov' = prov_vector[i],  b,"r.squared"=R_2,CI_lower, CI_upper,variance,"n"=nrow(fit$model), row.names = NULL)
    output_df <- rbind(output_df,curr_results_df)
  }
  return(output_df)
}

reg_prov <- function(input_df){
  city_prov_vector <- unique(input_df[,c("city", 'prov')])
  city_vector <- city_prov_vector[, 'city']
  prov_vector <- unique(city_prov_vector[, 'prov'])

  output_df <- data.frame()
  for (i in 1:length(prov_vector)){
    index <- which(input_df[, "prov"] == prov_vector[i])
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
    
    curr_results_df <- data.frame('prov' = prov_vector[i],  b,"r.squared"=R_2,CI_lower, CI_upper,variance,"n"=nrow(fit$model), row.names = NULL)
    output_df <- rbind(output_df,curr_results_df)
  }
  return(output_df)
}

reg_country <- function(input_df){
  # output_df <- data.frame()
  
    fit <- lm(y_temp~x_year, data = input_df)
    b <- data.frame("intercept" = fit$coefficients[1], "slope" = fit$coefficients[2])
    R_2 <- data.frame("r.squared" = as.numeric(unlist(summary(fit)$r.squared)))
    
    CIs <- ci(fit, 0.95, alpha=1-0.95, na.rm = TRUE)
    critical_value <- qt((1-0.95)/2, (nrow(fit$model)-1))
    standard_error <- summary(fit)$coef[,2][2]
    margin_error <- critical_value*standard_error
    estimate <- summary(fit)$coef[,1][2]
    CI_lower <-  estimate - margin_error
    CI_upper <- estimate + margin_error
    variance <- (standard_error)^2
    
    output_df <- data.frame(b,"r.squared"=R_2,CI_lower, CI_upper,variance,"n"=nrow(fit$model), row.names = NULL)
    # output_df <- b
  return(output_df)
}

# Interaction Model - Confirm Regression Results
# 
# city<- data.table(city_vector, stringsAsFactors = TRUE)
# fit_2 <- lm(y_temp~ city-1 + city*x_year , data = input_df)

#Overlay regression lines 
multiple_reg_lines <- function(city, prov){
  city_df <- output_df_all[ which(output_df_all$prov==prov
                                  & output_df_all$city == city), ]
  if(nrow(city_df) == 0)return(NULL)
  
  # city_df <- select(city_df, intercept, slope)
  debug(logger, paste('|CITY_DF |', city_df, "|"))
  output_df_prov <- reg_prov(input_df_all)
  prov_df <- output_df_prov[which(output_df_prov$prov==prov), ]
  # prov_df <- select(prov_df, intercept, slope)
  debug(logger, paste('|PROV_DF |', prov_df,"|"))
  country_df <- reg_country(input_df_all)
  debug(logger, paste('|CANADA_DF |', country_df,"|"))
  
  # horiz_city_1 <- city_df$intercept + city_df$CI_lower*1980
  # horiz_city_2 <- city_df$intercept + city_df$CI_upper*2020
  horiz_city_1 <- city_df$intercept + city_df$slope*1980
  horiz_city_2 <- city_df$intercept + city_df$slope*2020
  horiz_prov_1 <- prov_df$intercept + prov_df$slope*1980
  horiz_prov_2 <- prov_df$intercept + prov_df$slope*2020
  horiz_can_1 <- country_df$intercept + country_df$slope*1980
  horiz_can_2 <- country_df$intercept + country_df$slope*2020

  ylim <- c(round(min(horiz_city_1, horiz_prov_1, horiz_can_1, horiz_city_2, horiz_prov_2, horiz_can_2)),
            round(max(horiz_city_1, horiz_prov_1, horiz_can_1, horiz_city_2, horiz_prov_2, horiz_can_2)))

  
  # ylim <- c(round(min(horiz_city_1, horiz_city_2)), round(max(horiz_city_1, horiz_city_2)))

  plot <- plot(1, type="l", xlab="Year", ylab="Temperature", xlim=c(1980, 2020), ylim=ylim)
  abline(h=0, lty = 4)
  abline(a = city_df$intercept, b = city_df$slope, col = 'red', lwd = 3)
  abline(h=horiz_city_1, lty = 3, col = 'red')
  abline(a = prov_df$intercept, b = prov_df$slope, col = 'blue', lwd  = 3)
  abline(h=horiz_prov_1, lty = 3, col = 'blue')
  abline(a = country_df$intercept, b = country_df$slope, col = 'green', lwd  = 3)
  abline(h=horiz_can_1, lty = 3, col = 'green')
    # abline(a = city_df$intercept, b = city_df$slope, col = 'red', lwd = 2)
    # abline(a = city_df$intercept, b=city_df$CI_lower, lty = 2,lwd = 2, col = 'red')
    # abline(a = city_df$intercept, b=city_df$CI_upper,lty = 2, lwd = 2, col = 'red')
    # abline(a = prov_df$intercept, b = prov_df$CI_lower, col = 'blue', lty = 2,lwd  = 2)
    # abline(a = prov_df$intercept, b = prov_df$slope, col = 'blue', lwd  = 2)
    # abline(a = prov_df$intercept, b = prov_df$CI_upper, col = 'blue', lty = 2, lwd  = 2)
    # abline(a = country_df$intercept, b = country_df$CI_lower, col = 'green', lwd  = 2, lty = 2)
    # abline(a = country_df$intercept, b = country_df$slope, col = 'green', lwd  = 2)
    # abline(a = country_df$intercept, b = country_df$CI_upper, col = 'green', lwd  = 2, lty =2)

  legend("topright",
         legend = c(city, prov, 'CANADA'),
         col = c('red', 'blue', 'green'),
         cex = 1.2,
         lty=3:3)
  
  return(plot)
}

get_city_vector <- function(prov){
  if(file.exists(paste('RData/','constant_values','.RData'))){
    load(paste('RData/','constant_values','.RData'), .GlobalEnv)
    city_vector <- city_prov_vector[which(city_prov_vector$prov==prov), ]
    city_vector <- select(city_vector, city)
    return(city_vector)
  }
}

get_prov_vector <- function(temp_val, month, year_to_start){
  if(file.exists(paste('RData/','constant_values','.RData'))){
    load(paste('RData/','constant_values','.RData'), .GlobalEnv)
    prov_vector <- unique(city_prov_vector[, 'prov'])
    return(prov_vector)
  }
}


gg_multiple_reg_lines <- function(city, prov){
  city_df <- input_df_all[ which(input_df_all$prov==prov
                                  & input_df_all$city == city), ]
  if(nrow(city_df) == 0)return(NULL)
  
  debug(logger, paste('|GG_OVERLAY_SLOPES |'))
  prov_df <- input_df_all[which(input_df_all$prov==prov), ]
  # debug(logger, paste('|PROV_DF |', prov_df,"|"))
  country_df <- input_df_all
  # debug(logger, paste('|CANADA_DF |', country_df,"|"))
  
  plot <- ggplot() +
    geom_line(country_df, mapping = aes(x = x_year, y = y_temp, colour = "Country")) +
    geom_smooth(country_df, method = "lm", mapping = aes(x = x_year, y = y_temp, colour = "Country")) +
    geom_line(prov_df, mapping = aes(x = x_year, y=y_temp, colour = 'Province')) +
    geom_smooth(prov_df,mapping = aes(x = x_year, y=y_temp, colour = "Province"), method = "lm") +
    geom_line(city_df, mapping = aes(x = x_year, y=y_temp, colour = "City")) +
    geom_smooth(city_df,mapping = aes(x = x_year, y=y_temp, colour = "City"), method = "lm") +
    labs(x = "Year", y = "Temperature") 
  return(plot)
}


# hist(output_df_all$slope, freq = TRUE, main  = paste("Histogram of Slope(Canada)"), xlab = "Slope")
# abline(h=0, col = 'red')

hist_slope_prov <- function(prov){
    index <- which(output_df_all[, "prov"] == prov)
    prov_df <- output_df_all[index,]
    # Histogram with density plot and mean line 
  p<-ggplot(prov_df, aes(x=slope)) + 
    geom_histogram(aes(y=..density..), colour="black", fill="white")+
    geom_density(alpha=.2, fill="#FF6666") +
    geom_vline(aes(xintercept=mean(slope)),
                color="blue", linetype="dashed", size=1)
  return(p)
}

# # boxplot(output_df_all$slope~output_df_all$prov, xlab = 'Province',ylab = 'Slope', main = 'Boxplot of slope')
# boxplot(output_df_all$r.squared~output_df_all$prov, xlab = "Province", ylab= 'r.squared', main = 'Boxplot of R_2')
boxplot_val <- function(value){
  if(value == 'r.squared'){
    p<- ggplot(output_df_all, aes(x=prov, y=r.squared)) 
  }
  else if(value == 'slope'){
    p<- ggplot(output_df_all, aes(x=prov, y=slope)) 
  }

  p+geom_boxplot() +
    stat_summary(fun.y=mean, geom="point", shape=23, size=4)+
    stat_boxplot(geom = 'errorbar')
}

hist_slope <- function(){
  # Histogram with density plot and mean line 
  p<-ggplot(output_df_all, aes(x=slope)) + 
    geom_histogram(aes(y=..density..), colour="black", fill="white")+
    geom_density(alpha=.2, fill="#FF6666") +
    geom_vline(aes(xintercept=mean(slope)),
               color="blue", linetype="dashed", size=1)
  return(p)
}