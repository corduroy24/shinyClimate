# comments
# still buggy when a file/folder already exists

require(data.table)
require(tidyr)
library(plyr)
library(dplyr)
library(readr)
library(stringr)
library(gmodels)


 # dir = "Homog_monthly_min_temp"
 # temp_meas = list.files(path=dir, pattern="*.txt", full.names=TRUE)
 # i = 1;


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

load_cleaned_data <- function(year, month){
  provs <- c("AB","BC","YT","NT","NU","SK", "MB", "ON", "QC", "NB", "NS", "PE", "NL")
  combined_df <- data.frame()
  
  for (i in 1:length(txt_files_ls)){
    if(ns[i,3] == "AB.txt"){
      nprov <- unlist(strsplit(ns[i,3],'.txt'))
      ncity <- ns[i,2]
      # Non-breaking spaces...trim.white doesnt work... 
      txt_files_df <- read.table(file = txt_files_ls[i], header = TRUE, sep = " ",dec = ".", colClasses = "factor")
      years_greater<-txt_files_df[as.numeric(as.character(txt_files_df$Year))>=year_to_start,]
      y_temp <- suppressWarnings(as.numeric(as.character(unlist(years_greater[,month]))))
      x_year <- suppressWarnings(as.numeric(as.character(unlist(years_greater[,'Year']))))
      temp_df <- data.frame(y_temp, x_year, "city" = ncity, "prov" = nprov ) 
      combined_df <- rbind(combined_df, temp_df) 
    }
  }
  return(combined_df)
}

year_to_start <- 1980
month <- 'Feb'
input_df = load_cleaned_data(year_to_start, month)

output_df <- regression(input_df)

regression <- function(input_df){
  city_vector <- unique(input_df[,"city"])
  prov <- unique(input_df[,"prov"])
  output_df <- data.frame()
  # i = 1
  for (i in 1:length(city_vector)){
    index <- which(input_df[, "city"] == city_vector[i])
    fit <- lm(y_temp[index]~x_year[index], data = input_df)
    b <- data.frame("intercept" = fit$coefficients[1], "slope" = fit$coefficients[2])
    R_2 <- data.frame("r.squared" = as.numeric(unlist(summary(fit_1)$r.squared)))
    # CIs <- ci(fit, 0.95, alpha=1-0.95, na.rm = TRUE)
    critical_value <- qt((1-0.95)/2, (nrow(fit$model)-1))
    standard_error <- summary(fit)$coef[,2][2]
    margin_error <- critical_value*standard_error
    estimate <- summary(fit)$coef[,1][2]
    CI_lower <-  estimate - margin_error
    CI_upper <- estimate + margin_error
    variance <- (standard_error)^2
    curr_results_df <- data.frame("city"=city_vector[i],prov,b,"r.squared"=R_2,CI_lower, CI_upper,variance,"n"=nrow(fit$model), row.names = NULL)
    output_df <- rbind(output_df,curr_results_df) 
  }
  return(output_df)
}



# Interaction Model - Regression Results

#reproduce code, -> software engineering





city<- data.table(city_vector, stringsAsFactors = TRUE)
fit_2 <- lm(y_temp~ city-1 + city*x_year , data = input_df)



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
r_1 <- data.frame(city_matrix, intercept, slope, CI_lower, CI_upper, variance_slope, r.squared)
r_2 <- merge(x = df_n, y = r_1, by= 'city' , all  = TRUE)
# r_3 <- r_2[,c(1,3,4,7,8,2,5)]



# hist(r_2$slope, freq = TRUE, main  = "Histogram of Slope - NL", xlab = "Slope")
#
# hist(r_2$slope, prob = TRUE, main  = "Histogram of Slope - NL", xlab = "Slope")
# lines(density(r_2$slope), col = "red")
#
# # hist(x = CI_lower_slope, prob = TRUE=)
# # lines(density(CI_lower_slope), col = "red")
#
# write.csv(r_2,'reg_results_NL.csv')

# boxplot(reg_results_AB$slope~reg_results_AB$prov)

reg_results <- list(reg_results_AB, reg_results_BC, reg_results_MB, reg_results_NB, reg_results_NL, reg_results_NS, reg_results_NT, reg_results_NU, reg_results_ON, reg_results_PE, reg_results_QC, reg_results_SK, reg_results_YT)

names(reg_results[[1]]) <- names(reg_results[[2]])

combined_reg_results <- do.call("rbind", lapply(reg_results, as.data.frame))


boxplot(combined_reg_results$slope~combined_reg_results$prov)

# missing - CI plots, r.squared?
