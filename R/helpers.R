library(plyr)
library(data.table)
library(log4r)
library(tidyr)
library(gridExtra)
library(grid)
library(LaplacesDemon)
library(feather)

logger <- create.logger()
logfile(logger) <- 'debug.log'
level(logger) <- 'DEBUG'

# Purpose: Retrieve the necessary data 
# Input: @meas - temp, previously - min_temp, max_temp, mean_temp
#       @month: all months of the year 
#       @year_to_start: 
# Output: output_df_all
get_data <- function(meas, month, year_to_start){
  # cc <- list.files(pattern="*.RData", full.names = TRUE)
  # print(cc)
  # if(file.exists(paste('../RData/',meas,month, year_to_start,'.RData'))){
  if(file.exists(paste('RData/',meas,month, year_to_start,'.RData'))){
    # load(paste('../R/RData/',meas,month, year_to_start,'.RData'), .GlobalEnv)
    load(paste('RData/',meas,month, year_to_start,'.RData'), .GlobalEnv)
    debug(logger, paste("Rdata exists"))
  } else {
    # print("here_1")
    debug(logger, paste("RData does not exists"))
    # ugly...
    min_input_df_all <- load_cleaned_data(year_to_start, month, 'min_temp') #data matrix X
    output_df_all <- regression(min_input_df_all) #reg results 
    max_input_df_all <- load_cleaned_data(year_to_start, month, 'max_temp') #data matrix X
    output_df_all <- rbind(output_df_all, regression(max_input_df_all)) #reg results 
    mean_input_df_all <- load_cleaned_data(year_to_start, month, 'mean_temp') #data matrix X
    output_df_all <- rbind(output_df_all,regression(mean_input_df_all)) #reg results 
    input_df_all <- rbind(min_input_df_all, max_input_df_all, mean_input_df_all)
    save(output_df_all,input_df_all, file = paste('RData/',meas,month, year_to_start,'.RData'))
    # save(output_df_all,input_df_all, file = paste('../R/RData/',meas,month, year_to_start,'.RData'))
    # load(paste(meas,month, year_to_start,'.RData'), .GlobalEnv)
    load(paste('RData/',meas,month, year_to_start,'.RData'), .GlobalEnv)
  }
  return(output_df_all)
}


# Find data files 
find_meas_data <- function(meas){
  # debug(logger, paste("|IM HERE 2|"))
  # print("finding data...")
  # print(list.files('./Data'))
  if(meas == 'min_temp'){
    rda_files_ls = list.files(path="./Data/Homog_monthly_min_temp_cleaned", pattern="*.rds", full.names = TRUE)
    names = list.files(path="./Data/Homog_monthly_min_temp_cleaned", pattern="*.rds")
  }
  else if(meas == 'max_temp'){
    rda_files_ls = list.files(path="./Data/Homog_monthly_max_temp_cleaned", pattern="*.rds", full.names = TRUE)
    names = list.files(path="./Data/Homog_monthly_max_temp_cleaned", pattern="*.rds")
  }
  else if(meas == 'mean_temp'){
    rda_files_ls = list.files(path="./Data/Homog_monthly_mean_temp_cleaned", pattern="*.rds", full.names = TRUE)
    names = list.files(path="./Data/Homog_monthly_mean_temp_cleaned", pattern="*.rds")
  }
  # else if(meas == 'precip'){
  #   debug(logger, paste("|IM HERE 3|"))
  #   txt_files_ls = list.files(path="../Data/Adj_monthly_total_prec_cleaned", pattern="*.RData", full.names = TRUE)
  #   names = list.files(path="../Data/Adj_monthly_total_prec_cleaned", pattern="*.RData")
  # }
  path_names <- list(rda_files_ls, names) 
  # debug(logger, paste('|FIND TEMP DATA|'))
  return(path_names)
}

# Purpose: Load data from cleaning step
load_cleaned_data <- function(year_to_start = 1980, month = 'Feb', meas){
  # debug(logger, paste("|IM HERE 1|"))
  path_names <- find_meas_data(meas)
  rda_files_ls <- path_names[[1]]
  names <- path_names[[2]]
  ns = matrix(unlist(strsplit(names,'_',)),ncol = 3,byrow = TRUE)

  # build input data frame. 
  input_df <- data.frame()
  debug(logger, paste('|BEFORE FOR LOOP|'))
  
  for (i in 1:length(rda_files_ls)){
      nom_city <- ns[i,2]
      nom_prov <- unlist(strsplit(ns[i,3],'.rds'))
      rda_files_df <- readRDS(rda_files_ls[i])
      # rda_files_df <- readRDS(file = rda_files_ls[i], header = TRUE, sep = " ",dec = ".", colClasses = "factor")
      
      years_greater<-rda_files_df[as.numeric(as.character(rda_files_df$Year))>=year_to_start,]

      y_temp <- suppressWarnings(as.numeric(as.character(unlist(years_greater[,month]))))

      x_year <- suppressWarnings(as.numeric(as.character(unlist(years_greater[,'Year']))))
      # debug(logger, paste('|START YEAR|', year_to_start, '|'))
      temp_df <- data.frame(y_temp, x_year, "city" = nom_city, "prov" = nom_prov, 'meas_name' = meas)
      # debug(logger, paste('|LOAD CLEANED DATA|', 6, '|'))
      input_df <- rbind(input_df, temp_df)
  }
  return(input_df)
}



clean_user_data <- function(inFile){
  require(dplyr)
  df <- read.delim(inFile, skip = 0, header = FALSE, as.is=TRUE, dec=".", sep = ",", na.strings=c(" ", "",'NA'), strip.white = TRUE)
  stationNum_city_prov <- paste(select(df, V1)[1,1], trimws(select(df, V2)[1,1]), province <- select(df, V3)[1,1], sep="_")
  stationNum_city_prov<- stringr::str_replace_all(stationNum_city_prov, "/",'-')
  seq(from = 3, to = 35, by = 2)
  # print(inFile)
  df <- select(df, -seq(from = 3, to = 35, by = 2))
  data <- slice(df, 5:n())
  (hdr <- slice(df, 3))
  is.na(hdr)
  
  df <- plyr::rename(data, hdr)
  #filter out -9999.9 - default values
  df <- data.frame(lapply(df, function(x){
    gsub("-9999.9", "NA", x)
  }))
  
  # build input data frame. 
  input_df <- data.frame()
  debug(logger, paste('|BEFORE FOR LOOP|'))
  
  for (i in 1:length(rda_files_ls)){
    nom_city <- ns[i,2]
    nom_prov <- unlist(strsplit(ns[i,3],'.rds'))
    rda_files_df <- readRDS(rda_files_ls[i])
    # rda_files_df <- readRDS(file = rda_files_ls[i], header = TRUE, sep = " ",dec = ".", colClasses = "factor")
    
    years_greater<-rda_files_df[as.numeric(as.character(rda_files_df$Year))>=year_to_start,]
    
    y_temp <- suppressWarnings(as.numeric(as.character(unlist(years_greater[,month]))))
    
    x_year <- suppressWarnings(as.numeric(as.character(unlist(years_greater[,'Year']))))
    # debug(logger, paste('|START YEAR|', year_to_start, '|'))
    temp_df <- data.frame(y_temp, x_year, "city" = nom_city, "prov" = nom_prov, 'meas_name' = meas)
    # debug(logger, paste('|LOAD CLEANED DATA|', 6, '|'))
    input_df <- rbind(input_df, temp_df)
  }
  return(input_df)
  
  # df
}


#########################################################
# Purpose: Perform Regression 
# Input: input_df, containing years, temp, city, prov 
# Output: statistical data 
#########################################################
regression <- function(input_df){
  city_prov_vector <- unique(input_df[,c("city", 'prov')])
  city_vector <- city_prov_vector[, 'city']
  prov_vector <- city_prov_vector[, 'prov']
  meas <- unique(input_df$meas_name)
  output_df <- data.frame()
  
  for (i in 1:nrow(city_prov_vector)){
    index <- which(input_df$prov==prov_vector[i]
                   & input_df$city == city_vector[i])
    fit <- lm(y_temp[index]~x_year[index], data = input_df)
    b <- data.frame("intercept" = fit$coefficients[1], "slope" = fit$coefficients[2])
    R_2 <- data.frame("r.squared" = as.numeric(unlist(summary(fit)$r.squared)))
    # CIs <- ci(fit, 0.95, alpha=1-0.95, na.rm = TRUE)
    critical_value <- qt((1-0.95)/2, (nrow(fit$model)-1))
    standard_error <- summary(fit)$coef[,2][2]
    margin_error <- critical_value*standard_error
    estimate <- summary(fit)$coef[,1][2]
    CI_lower <-  estimate + margin_error
    CI_upper <- estimate - margin_error
    variance <- (standard_error)^2
    curr_results_df <- data.frame("city"=city_vector[i],'prov' = prov_vector[i],  
                                  b,"r.squared"=R_2,CI_lower, CI_upper,variance,
                                  "n"=nrow(fit$model), 'meas_name' = meas,  row.names = NULL)
    output_df <- rbind(output_df,curr_results_df)
  }
  return(output_df)
}

# Interaction Model - Confirm Regression Results
# city<- data.table(get_city_vector('ON'), stringsAsFactors = TRUE)
# fit_2 <- lm(y_temp~city-1+city*x_year-x_year , data = input_df)

# # Draw plots
# df_consts <- data.frame(year_to_start <- '1980',
# plot_type <- 'regression line',
# location <- 'CANADA',
# region <- 'Province',
# stat<- 'Slopes',
# meas<-'min_max_temp',
# month <-'Jan',
# city <- 'TORONTO',
# prov <- 'ON',
# city_lab <-'Enable')



##################################################################
# Purpose: Modularize plotting - Mediator for other plot function
# Input: meas, month, dataframe containing more variables 
# output: grid drawn on UI, and grob object 
#################################################################
setup_plots <- function(meas, month, df_consts, grouping){
  year_to_start <- df_consts$year_to_start
  plot_type <- df_consts$plot_type
  location <- df_consts$location
  region <- df_consts$region
  stat<- df_consts$statistic
  city <- df_consts$city
  prov <- df_consts$prov
  city_lab <- df_consts$prov
  # print(city)
  # print(prov)
  # debug(logger, paste('-----------df_consts ----------',df_consts ))
  output_df_all <- get_data('temp', month, year_to_start)
  prov_vector <- c("ON","AB","BC","YT","NT","NU","SK", "MB", "QC", "NB", "NS", "PE", "NL")
  
  
  # if(grouping  != 'none'){
  #   print(grouping)
  #   north_prov <- c('YT', 'NT', 'NU')
  #   north_df <- output_df_all[output_df_all$prov %in% north_prov,]
  #   south_df <- output_df_all[!(output_df_all$prov %in% north_prov),]
  #   if(grouping == 'north')
  #     p<-add_plot_data(meas, north_df) # returns a list plot(s)
  #   else if(grouping  == 'south')
  #     p<-add_plot_data(meas, south_df) # returns a list plot(s)
  #   
  #   p<-add_plot_type(p, df_consts) #constructs plot(s)
  #   # print(p)
  #   p<- create_grid(p,month, df_consts)
  #   suppressMessages( grid.draw(p))
  #   return(invisible(p))
  # }
  # else{
    if(region == 'Province'){
      index <- which(output_df_all[, "prov"] == location)
      output_df_all <- output_df_all[index,]
    }
    else if(region == 'City'){
      # city <- strsplit(location, ',')[[1]][1]
      # prov <- strsplit(location, ',')[[1]][2]
      index <- which(input_df_all$prov==prov
                     & input_df_all$city == city)
      output_df_all <- input_df_all[index,] # chnage name.. .
    }
  else if(region == 'North'){
      north_prov <- c('YT', 'NT', 'NU')
      output_df_all <- output_df_all[output_df_all$prov %in% north_prov,]
  }
    
    p<-add_plot_data(meas, output_df_all) # returns a list plot(s)
    p<-add_plot_type(p, df_consts) #constructs plot(s)
    # print(p)
    p<- create_grid(p,month, df_consts)
    suppressMessages( grid.draw(p))
    invisible(p)
  
}


add_plot_data <- function(meas, output_df_all){
  
  if(meas == 'min_max_temp'){
    min_output_df_all <- output_df_all[which(output_df_all$meas_name=='min_temp'),]
    max_output_df_all <- output_df_all[which(output_df_all$meas_name=='max_temp'),]
    p1<-ggplot(min_output_df_all) 
    p2<-ggplot(max_output_df_all) 
    
    return(list(p1,p2))
  }
  else if(meas == 'mean_temp'){
    mean_output_df_all <- output_df_all[which(output_df_all$meas_name=='mean_temp'),]
    p<-ggplot(mean_output_df_all) 
    return(list(p))
  }
}

####################################################
# Purpose: setup plot types, such as histogram or boxplot 
################################################
add_plot_type<- function(curr_plots, df_consts){
  plot_type <- df_consts$plot_type
  region <- df_consts$region
  stat <- df_consts$stat
  city <- df_consts$city
  prov <- df_consts$prov
  show_city_lab <- df_consts$show_city_lab
  year_to_start <- df_consts$year_to_start

  stat_lab <-bquote(.(stat)*' ('*degree *'C per year)')
  # print(strsplit(stat, ' ')[[1]][1])
  
  for(i in 1: length(curr_plots)){
    dat <- curr_plots[[i]]$data
    # dat_city <- NULL
    # x_dat <- NULL
    # if(dat$prov==prov && dat$city == city){
      index <- which(dat$prov==prov& dat$city == city)
      dat_city <- dat[index,] 
    # }
    print(plot_type)
    if(plot_type == 'histogram'){
      aes <- aes(x = slope)
      # aes_vline<- aes(xintercept=mean(slope))
      x_city <- dat_city$slope 
      x_dat <- mean(dat$slope)
      if(strsplit(stat, ' ')[[1]][1] == 'R\U000B2'){
        # stat_lab <-bquote(R^2 *' ('*degree *'C per year)')
        stat_lab <-bquote(R^2)
        aes <- aes(x = r.squared)
        # aes_vline<-aes(xintercept=mean(r.squared))
        x_city <- dat_city$r.squared 
        x_dat <- mean(dat$r.squared)
      }
      else if(strsplit(stat, ' ')[[1]][1] == 'Lower.Bound'){
        aes <- aes(x = CI_lower)
        # aes_vline<-aes(xintercept=mean(CI_lower))
        x_city <- dat_city$CI_lower 
        x_dat <- mean(dat$CI_lower)
      }
      else if(strsplit(stat, ' ')[[1]][1] == 'Upper.Bound'){
        aes <- aes(x = CI_upper); 
        # aes_vline<-aes(xintercept=mean(CI_upper))
        x_city <- dat_city$CI_upper
        x_dat <- mean(dat$CI_upper)
      }
        
      curr_plots[[i]] <- curr_plots[[i]]+ aes +
        geom_histogram(aes(y=..density..), colour="black", fill="white")+
        geom_density(alpha=.05, fill="#FF6666") +
        # geom_vline(aes_vline,color="blue", linetype="dashed", size=1)+
        labs(y='Frequency', x = stat_lab)

      if(show_city_lab == 'Enable')
        curr_plots[[i]] <- curr_plots[[i]] +
        geom_point(x = x_city ,y = 5, colour = 'purple')+
        annotate("text", x = x_dat, y = 10  , vjust = 1, hjust = 1,
                 label = str_replace(city," ","_"), parse = TRUE, colour= 'purple')

    }
    else if(plot_type == 'boxplot'){
      aes <- aes(x=prov, y=slope) #For slope.. 
      if(strsplit(stat, ' ')[[1]][1] == 'R\U000B2'){
        stat_lab <-bquote(R^2)
        aes <- aes(x=prov, y=r.squared)
        # print("HEREEEEEE------------------")
      }
      curr_plots[[i]] <- curr_plots[[i]] + aes +
        geom_boxplot() +
        stat_summary(fun.y=mean, geom="point", shape=23, size=4)+
        stat_boxplot(geom = 'errorbar')+
        geom_hline(yintercept=0, linetype="dashed", color = "red", size = 1.2)+
        labs(y=stat_lab, x = 'Province')
    }
    else if(plot_type == 'regression line'){
      dat <- curr_plots[[i]]$data
      aes <- suppressMessages(aes(x = x_year, y = y_temp))
      fit <- suppressMessages(lm(y_temp~x_year, data = dat))
      R_2 <- as.numeric(unlist(summary(fit)$r.squared))
      # print(R_2)
      critical_value <- qt((1-0.95)/2, (nrow(fit$model)-1))
      standard_error <- summary(fit)$coef[,2][2]
      margin_error <- critical_value*standard_error
      estimate <- summary(fit)$coef[,1][2]
      CI_lower <-  estimate + margin_error
      CI_upper <- estimate - margin_error
      curr_plots[[i]] <- curr_plots[[i]]+ aes +
        geom_point(size = 1)+
        stat_smooth(method = 'lm', se = FALSE)+
        labs(y=stat_lab, x = 'Years')+
        scale_x_continuous(breaks  = seq(year_to_start,2020, by = 5))
      
      curr_plots[[i]] <- curr_plots[[i]] +
        ggpubr::stat_regline_equation(label.x.npc = 'left', label.y.npc='bottom', colour= 'purple')+
        annotate("text", x = 1985, y = -Inf  , vjust = -0.5,
                 label = paste('R^2 == ', signif(R_2,2)), parse = TRUE, colour= 'purple')+
        annotate("text", x = Inf, y = -Inf  , vjust = -0.5, hjust= 1.5,
                 label = paste('CI = (', signif(CI_lower,2), ',',signif(CI_upper,2),')') , colour= 'purple')

    }
  }
  return(curr_plots)
}

##########################################
# Purpose: adding plot labels
# Input: curr_plot, title, meas, stat
# Output:
##########################################
add_grid_labels <- function(curr_plot, title_meas, stat){
  title  = bquote(.(title_meas)*' - '~.(stat))
  if (stat == "Temperatures vs Years")
    title  = bquote(.(title_meas) *' vs Years')
  curr_plot <- curr_plot + 
    ggtitle(title)+
    theme(plot.title = element_text(hjust = 0.5, size = 10),
          axis.title.x = element_text(size = 9),
          axis.title.y = element_text(size = 9))
}

#############################################################################
# Purpose: creating grid for displaying both min and max plots on same panel
##########################################################################
create_grid <-function(curr_plot, month, df_consts){
  year_to_start <- toString(df_consts$year_to_start)
  location <- df_consts$location
  stat <- df_consts$stat
  region <- df_consts$region
  print(location)
  month <- toString(month)
  year_end <- switch(region, 'City' = max(curr_plot[[1]]$data$x_year), {'2017'}) 
  subt<- bquote(italic(.(location)*' -'~.(month) *' - ('* .(year_to_start)*' - '* .(year_end) *')'))
  if(strsplit(stat, ' ')[[1]][1] == 'R\U000B2')
    stat <- bquote(R^2)
  # if its Min_max_temp 
  if (length(curr_plot) == 2){
    curr_plot[[1]] <- add_grid_labels(curr_plot[[1]], 'Minimum Temperatures', stat)
    curr_plot[[2]]<- add_grid_labels(curr_plot[[2]], 'Maximum Temperatures', stat)
    title = bquote('Min. vs Max. Temperatures - '~.(stat))
    
    if(stat == 'Temperatures vs Years')
      title = bquote('Min. vs Max. Temperatures')
    
    p<- arrangeGrob(
      top = textGrob(title, gp=gpar(fontface="bold")),
      sub = textGrob(subt, gp = gpar(col = 'red', fontface='italic',
                                     fontsize = 11 )),
      curr_plot[[1]],
      curr_plot[[2]],
      bottom = textGrob(
        "Source: Environment Canada Temperature Data - 2017",
        gp = gpar(fontface = 3, fontsize = 9),
        hjust = 1,
        x = 1
      ),
      ncol = 1,
      heights=c(0.05, 0.5, 0.55)
    )
  } # add caption...
  else if(length(curr_plot) == 1){
    title = bquote('Mean Temperatures - '~.(stat))
    if (stat == "Temperatures vs Years")
      title = 'Mean Temperatures vs Years'
                     
    p<- curr_plot[[1]]+
      ggtitle(title)+
      labs(subtitle = subt)+
      theme(plot.title = element_text(hjust = 0.5),
            plot.subtitle = element_text(hjust = 0.5, color = 'red' ))
  }
  return(p)
}

###################################################
# Purpose: get names of city for a given province 
# input: prov 
# output: vector with names of city 
###################################################
get_city_vector <- function(prov){
  # print(getwd())
  # cc <- list.files(full.names = TRUE)
  # print(cc)
  if(file.exists(paste('RData/', 'constant_values','.RData'))){
    # print("exists!!!")
    load(paste('RData/','constant_values','.RData'), .GlobalEnv)
    city_vector <- city_prov_vector[which(city_prov_vector$prov==prov), ]
    city_vector <- dplyr::select(city_vector, city) 
    # city_vector <- data.frame(city_vector[, 'city'])
    city_vector$city <- as.character(city_vector$city)
    city_v <- sort(city_vector$city)
    return(city_v)
  }
}

get_city_stats <- function(month, df_consts){
  city<- df_consts$city
  prov <- df_consts$prov
  year_to_start <- df_consts$year_to_start
  output_df_all <- get_data('temp', month, year_to_start)

  # if(region == 'City'){
    index <- which(output_df_all$prov==prov
                   & output_df_all$city == city)
    output_df_all <- output_df_all[index,]
  # }
  
  # if(meas == 'min_max_temp'){
  #   min_output_df_all <- output_df_all[which(output_df_all$meas_name=='min_temp'),]
  #   max_output_df_all <- output_df_all[which(output_df_all$meas_name=='max_temp'),]
  #   slope_min <- min_output_df_all$slope
  #   slope_max <- max_output_df_all$slope
  #   return(list(p1,p2))
  # }
  # else if(meas == 'mean_temp'){
  #   mean_output_df_all <- output_df_all[which(output_df_all$meas_name=='mean_temp'),]
  #   slope_mean <- max_output_df_all$slope
  #   return(list(p))
  # }
  

} 




new_des_block_city<- function(number,res, name){
  color = NULL
  icon = NULL
  if(name == 'SLOPE'){
    color <- evaluate_color(number)
    icon <- evaluate_icon(number)
    number  <- paste0(number, ' \U00B0','C per year')
  }
  else if(name == 'R\U000B2')
    number <- paste0(number*100, '%')
  else if(name  == 'CONFIDENCE INTERVAL')
    number  <- paste0(number, ' \U00B0','C per year')
  

  descriptionBlock(
    number = number, 
    number_color = color, 
    number_icon = icon,
    header = res, 
    text = name, 
    right_border = TRUE,
    margin_bottom = FALSE
  )
}

new_des_block_prov_can<- function(number, res, name){
  color = NULL
  icon = NULL
  # if(name == 'SLOPE'){
  #   color <- evaluate_color(number)
  #   icon <- evaluate_icon(number)
  #   number  <- paste0(number, ' \U00B0','C per year')
  # }
  # else if(name == 'R\U000B2')
  #   number <- paste0(number*100, '%')
  # else if(name  == 'CONFIDENCE INTERVAL')
  #   number  <- paste0(number, ' \U00B0','C per year')
  
  descriptionBlock(
    # number = paste('Central Tendacy:', signif(number$mean,2),'median:',
    #                signif(number$med,2), 'mode:',signif(number$mode,2) ),
    # number = 
    # number_color = color,
    # number_icon = icon,
    header = res, 
    text = name,
    right_border = TRUE,
    margin_bottom = FALSE
  )
}
# city_month_eval<- function(min, max){
#   if(min)
#   if(min$slope_res == 'Decreasing relatively fast'){
#     if(max$slope_res == 'Decreasing relatively fast'){
#       result_1 <- ''
#     }
#     else if (max$slope_res == 'Decreasing relatively slow'){}
#     else if (max$slope_res == 'Increasing relatively fast' ){}
#     else if (max$slope_res == 'Increasing relatively slow'){}
#   }
#   else if (min$slope_res == 'Decreasing relatively slow'){
#     if(max$slope_res == 'Decreasing relatively fast'){}
#     else if (max$slope_res == 'Decreasing relatively slow'){}
#     else if (max$slope_res == 'Increasing relatively fast' ){}
#     else if (max$slope_res == 'Increasing relatively slow'){}
#   }
#   else if (min$slope_res == 'Increasing relatively fast' ){
#     if(max$slope_res == 'Decreasing relatively fast'){}
#     else if (max$slope_res == 'Decreasing relatively slow'){}
#     else if (max$slope_res == 'Increasing relatively fast' ){}
#     else if (max$slope_res == 'Increasing relatively slow'){}
#   }
#   else if (min$slope_res == 'Increasing relatively slow'){
#     if(max$slope_res == 'Decreasing relatively fast'){}
#     else if (max$slope_res == 'Decreasing relatively slow'){}
#     else if (max$slope_res == 'Increasing relatively fast' ){}
#     else if (max$slope_res == 'Increasing relatively slow'){}
#   }
#         
# 
# 
#   
# }


eval_hist <- function(month, df_consts){
  city<- df_consts$city
  prov <- df_consts$prov
  year_to_start <- df_consts$year_to_start
  region <- df_consts$region
  output_df_all <- get_data('temp', month, year_to_start)

  if(region == "Province"){
    index <- which(output_df_all$prov==prov)
    output_df_all <- output_df_all[index,]
  }

  slope_eval <- stat_eval('slope', output_df_all)
  lower_eval <- stat_eval('CI_lower', output_df_all)
  upper_eval<- suppressWarnings( stat_eval('CI_upper', output_df_all))
  r2_eval<- stat_eval('r.squared', output_df_all)
  list <- list(slope = slope_eval, lower =lower_eval, upper = upper_eval, r2 = r2_eval)
    
  
  return(list)
} 

stat_eval<-function(stat, output_df_all){
  
  min_output_df_all <- output_df_all[which(output_df_all$meas_name=='min_temp'),]
  max_output_df_all <- output_df_all[which(output_df_all$meas_name=='max_temp'),]
  # mean_output_df_all <- output_df_all[which(output_df_all$meas_name=='mean_temp'),]
  # print(LaplacesDemon::is.bimodal(min_output_df_all$slope))
  skewness<- round(e1071::skewness(min_output_df_all[,stat]))
  # print(skewness)
  
  if(LaplacesDemon::is.bimodal(min_output_df_all[,stat]))
    min_result <- 'bimodal'
  else if(skewness > 1 )
    min_result<-'highly right-skewed'
  else if(skewness < (-1))
    min_result<-'highly left-skewed'
  else if(skewness >= (-1) && skewness < (-0.5) )
    min_result<-'moderately left-skewed'
  else if(skewness <= (1) && skewness >=(0.5) )
    min_result<-'moderately right-skewed'
  else if(skewness >= (-0.5) && skewness <= 0.5)
    min_result<-'approximately symmetric'
  else
    min_result<-"undetected"
  
  # print(LaplacesDemon::is.bimodal(max_output_df_all$slope))
  skewness<- round(e1071::skewness(max_output_df_all[,stat]),2)
  # print(skewness)
  
  if(LaplacesDemon::is.bimodal(max_output_df_all[,stat]))
    max_result<-'bimodal'
  else if(skewness > 1 )
    max_result<-'highly right-skewed'
  else if(skewness < (-1))
    max_result<-'highly left-skewed'
  else if(skewness >= (-1) && skewness <= (-0.5) )
    max_result<-'moderately left-skewed'
  else if(skewness <= (1) && skewness >=(0.5) )
    max_result<-'moderately right-skewed'
  else if(skewness >= (-0.5) && skewness <= 0.5)
    max_result<-'approximately symmetric'
  else
    max_result<-"undetected"
  
  density_estimate_1 <- density(min_output_df_all[,stat])
  min_mode <- signif(density_estimate_1$x[which.max(density_estimate_1$y)],1)
  # print(min_mode)
  density_estimate_1 <- density(max_output_df_all[,stat])
  max_mode <- signif(density_estimate_1$x[which.max(density_estimate_1$y)],1)
  # print(max_mode)
  values_min = list(mean = mean(min_output_df_all[,stat]), med = median(min_output_df_all[,stat]), mode = min_mode)
  values_max = list(mean = mean(max_output_df_all[,stat]), med = median(max_output_df_all[,stat]), mode = max_mode)
  
  values = list(min = values_min, max = values_max) 
  new_df <- list(value = values, min = min_result,max =  max_result)
  # df<- rbind(df, new_df)
}
test<- function(){

index <- which(output_df_all$prov=='MB')
output_df_all <- output_df_all[index,]
  min_output_df_all <- output_df_all[which(output_df_all$meas_name=='min_temp'),]

get_mode(min_output_df_all)
}

get_mode <- function(v) {
  uniqv <- unique(v$slope)
  uniqv[which.max(tabulate(match(v$slope, uniqv)))]
}

evaluate_color<- function(stat){
  # print(typeof(stat))
  if(stat < 0) result <- 'red'
  else if (stat > 0) result <- 'green' 
  return(result)
}
evaluate_icon <- function(stat){
  if(stat < 0) result <- 'fa fa-caret-down'
  else if (stat > 0) result <- 'fa fa-caret-up' 
  else result <- NULL
  return(result)
}
evaluate_slope <- function(slope){
  if(slope < 0) {
    if(-(slope) > 0.099)
      result <- 'Decreasing relatively fast'
    else 
      result <- 'Decreasing relatively slow'
  }
  else if (slope > 0){
    if(slope > 0.099)
      result <- 'Increasing relatively fast' 
    else 
      result <- 'Increasing relatively slow'
  }
  else result <- 'neither'
  return(result)
}

evaluate_ci <- function(lower, upper){
  #hypothesis test 
  # strong evidence or significant evidence
  if(lower < 0 && upper < 0) result <- 'Slope is significant'
  else if (lower < 0 && upper > 0) result <- 'Slope is not significant'
  else if (lower > 0 && upper > 0) result <-  'Slope is significant'
  return(result)
}


evaluate_r2 <- function(slope){
  
  if(slope == 1)
    result <- 'Model is strong. Model explains all temperature variability '
  else if(slope == 0)
    result <- 'Model is inadmissable. Explains none of the temperature variability '
  else if (slope > 0.5)
    result <- 'Model is good. Explains most of the temperature variablity'
  else if(slope >=  0.1)
    result <- 'Model is acceptable. Explains some temperature variability '
  else if (slope < 0.1)
    result <- 'Model is weak. Explains a small amount of temperature variablity'
  
  return(result)
}

######################################################
######################################################
# The following functions are not needed for the app 
# extreme_warming
# clean_data
# check_start_year_cutoff
# get_prov_vector(
# map
#####################################################
#####################################################


# PurposeL dummy algorithm for finding extreme warming within a province
# extreme_warming <- function(curr_prov, up_bound){
#   # up_bound <- 10
#   # curr_prov <- 'NB'
#   month_1 <- 'Jan'
#   month_2 <- 'Jul'
#   year_to_start <- '1980'
#   output_df_all <- get_data('temp', month_1, year_to_start)
#   
#   tt <- output_df_all[which(output_df_all[, "prov"] == curr_prov),]
#   
#   tt_min <- tt[which(tt[, "meas_name"] == 'min_temp'),]
#   tt_max <- tt[which(tt[, "meas_name"] == 'max_temp'),]
#   tt_min_sorted <- tt_min[order(-tt_min$slope),]
#   tt_max_sorted <- tt_max[order(-tt_max$slope),]
#   top_15_min <- slice(tt_min_sorted , 1:up_bound)
#   top_15_max <- slice(tt_max_sorted, 1:up_bound)
#   result_1 <- merge(top_15_min, top_15_max, by = 'city')
#   
#   output_df_all <- get_data('temp', month_2, year_to_start)
#   tt <- output_df_all[which(output_df_all[, "prov"] == curr_prov),]
#   
#   tt_min <- tt[which(tt[, "meas_name"] == 'min_temp'),]
#   tt_max <- tt[which(tt[, "meas_name"] == 'max_temp'),]
#   tt_min_sorted <- tt_min[order(-tt_min$slope),]
#   tt_max_sorted <- tt_max[order(-tt_max$slope),]
#   top_15_min <- slice(tt_min_sorted , 1:up_bound)
#   top_15_max <- slice(tt_max_sorted, 1:up_bound)
#   result_2 <- merge(top_15_min, top_15_max, by = 'city')
# 
#   
#   return(result_3 <- merge(result_1, result_2, by = 'city'))
#   
# }


# Purpose: Data cleaning step
# Input: @var, @dir
# Output: No return; writes to file
# Not needed for app...
# dir = list.files(path="./Data/Homog_monthly_min_temp", pattern="*.txt", full.names = TRUE)
# var = list.files(path="./Data/Homog_monthly_min_temp", pattern="*.txt")

# meas_dir = "./R/Data/Adj_monthly_total_prec"
# meas_city_data = list.files(path=meas_dir, pattern="*.txt", full.names=TRUE)
# clean_data_target <- function(meas_city_data, meas_dir){
#   require(dplyr)
#   for (i in 1:length(meas_city_data)){
#   # for (i in 1:2){
#     df = read.delim(meas_city_data[i], skip = 0, header = FALSE, as.is=TRUE, dec=".", sep = ",", na.strings=c(" ", "",'NA'), strip.white = TRUE)
#     stationNum_city_prov <- paste(select(df, V1)[1,1], trimws(select(df, V2)[1,1]), province <- select(df, V3)[1,1], sep="_")
#     #forward slash for precipatation files - "7025250_MONTREAL/PIERRE ELLIOTT T_QC"
#     stationNum_city_prov<- stringr::str_replace_all(stationNum_city_prov, "/",'-')
#     seq(from = 3, to = 35, by = 2)
#     # print(meas_city_data[i])
#     df <- select(df, -seq(from = 3, to = 35, by = 2))
#     data <- slice(df, 5:n())
#     (hdr <- slice(df, 3))
#     is.na(hdr)
# 
#     df <- plyr::rename(data, hdr)
#     #filter out -9999.9 - default values
#     df <- data.frame(lapply(df, function(x){
#       gsub("-9999.9", "NA", x)
#     }))
# 
#     filePath = paste0(meas_dir,"_cleaned/", stationNum_city_prov,".rds")
#     # filePath= sprintf("%s_cleaned/%s.txt",meas_dir,stationNum_city_prov)
# 
#     saveRDS(df, filePath)
#   }
# }


# folder_path = "./Data/Surface N2O from NOAA"
# folder_path = "./Data/Surface MH4 from NOAA"
folder_path = "./Data/Surface CO2 from NOAA"
file_path_ls = list.files(path=folder_path, pattern="*.txt", full.names=TRUE)
file_name_ls = list.files(path=folder_path, pattern="*.txt")
clean_data_feature <- function(file_path_ls, folder_path, file_name_ls){
  require(dplyr)
  for (i in 1:length(file_path_ls)){
    # for (i in 1:2){
    # i=1
    # print(file_path_ls[i])
    df = read.delim(file_path_ls[i], skip = 0, header = FALSE, as.is=TRUE, dec=".", sep = ",", na.strings=c(" ", "",'NA'), strip.white = TRUE)
    (num_header_lines<- sub(".*: ", "", df[1,1]) %>% as.numeric())
    df <- read.delim(file_path_ls[i], skip = (num_header_lines-1) , header = FALSE, as.is=TRUE, dec=".", sep = "", na.strings=c(" ", "",'NA'), strip.white = TRUE)
    
    (hdr <- c(df[1,4], df[1,5], df[1,6]) %>% str_to_title)
    df <- df %>% select(2:4) %>% slice(2:n())
    
    is.na(hdr)
    names(df) <- hdr
    df$Month <- plyr::mapvalues(df$Month, 
                                from=c("12","11","10",'9', '8', '7', '6', '5', '4','3', '2','1'), 
                                to=c("Dec","Nov","Oct", 'Sep', 'Aug', 'Jul', 'Jun', 'May', 'Apr', 'Mar', 'Feb', 'Jan'))
    # head(alt_df)
    # print(file_name_ls[i])
    file_name <- sub(".txt.*", "", file_name_ls[i])
    # print(file_name)
    # print(sum(is.na(df)))
    new_filePath = paste0(folder_path,"_cleaned/", file_name,".rds")

    saveRDS(df, new_filePath)
  }
}
  
# check_start_year_cutoff <- function(meas){
#   temp_object <- find_meas_data(meas)
#   txt_files_ls <- temp_object[[1]]
#   names <- temp_object[[2]]
#   most_recent_year <-c()
#   for (i in 1:length(txt_files_ls)){
#     txt_files_df <- read.table(file = txt_files_ls[i], header = TRUE, sep = " ",dec = ".", colClasses = "factor")
#     x_temp <- suppressWarnings(as.numeric(as.character(unlist(txt_files_df[,'Year']))))
#     most_recent_year[i] <- max(x_temp)
#   }
#   most_recent_year <- most_recent_year[!is.na(most_recent_year)]
#   # debug(logger, paste('|most_recent_year ' , '|', most_recent_year,"|"))
#   start_year_cutoff <- min(most_recent_year, na.rm = TRUE)
#   # debug(logger, paste('|min ' , '|',start_year_cutoff,"|"))
#   return(start_year_cutoff)
# }

# get_prov_vector <- function(meas, month, year_to_start){
#   if(file.exists(paste('/RData/','constant_values','.RData', sep=''))){
#     load(paste('/RData/','constant_values','.RData', sep=''), .GlobalEnv)
#     prov_vector <- unique(city_prov_vector[, 'prov'])
#     prov_vector <- prov_vector[ , order(names(prov_vector))]
#     return(prov_vector)
#   }
# }

# library(sfb)
# library(raster)
# map <- function(){
#   # can0 <- getData("GADM",country="CAN",level=0)
#   provinces <- c("Ontario")
#   # can1 <- getData('GADM', country="CAN", level=1)
#   # can1 <- readRDS("gadm36_CAN_1_sp.rds")
#   # ca.provinces <- can1[can1$NAME_1 %in% provinces,]
#   # can2<-getData('GADM', country="CAN", level=2) # counties
#   can2 <- readRDS("C:/Environment_Canada_Shiny_App/gadm36_CAN_2_sp.rds")
#   ca.cities <- can2[can2$NAME_1 %in% provinces,]
#   prov <- 'ON'
#   prov_df_city_slope <- output_df_all[ which(output_df_all$prov==prov),]
#   prov_df_city_slope <- prov_df_city_slope[,c('city', 'slope')]
#   prov_df_city_slope$city <- str_to_title(prov_df_city_slope$city)
# 
#   munic_div<- read.csv('C:/Environment_Canada_Shiny_App/Data/mmah-list-of-ontario-municipalities-en-utf8-2020-01-03_0.csv')
#   munic_div$Municipality <- gsub("<.*?>","",as.character(munic_div$Municipality))
#   temp <- munic_div$Municipality
#   temp <- gsub(",.*", "", temp)
#   munic_div$Municipality <- temp
#   prov_df_temp <- merge(munic_div, prov_df_city_slope, by.x ='Municipality', by.y ="city")
#  
#   ca.cities@data$id <- rownames(ca.cities@data)
#   
#   prov_df <- merge(ca.cities@data,prov_df_temp , by.x = 'NAME_2', by.y = 'Geographic.area')
# 
#   check<-st_as_sf(ca.cities)
#   
#   check5 <- merge(check, munic_div, by.x = 'NAME_2', by.y = 'Geographic.area')
#   check7 <- merge(check5, prov_df_city_slope, by.x = 'Municipality', by.y = 'city')
#   
#   copy_check7 <- check7 
#   st_geometry(copy_check7) <- NULL
#   unique_copy_check7_slope <- data.frame(unique(copy_check7$NAME_2))
#   new_new <-data.frame()
#   for(i in 1: nrow(unique_copy_check7_slope)){
#     name <- unique_copy_check7_slope[i,]
#     index <- copy_check7[which(copy_check7$NAME_2 == name),]
#     mean_slope <- mean(index$slope)
#     index$slope <- mean_slope
#     new_new <- rbind(new_new, index)
#   }
#   check7$slope <-new_new$slope
# 
#     gg<- ggplot(data = check7)+
#       geom_sf(aes(fill= slope))+
#       scale_fill_gradient(name = 'Trends',
#                           low = "blue", high = "gold2")
#      gg+ geom_path(data= ca.cities, mapping = aes(x=long, y =lat, group = group))
#     # return(base_sp)
# }
# 
