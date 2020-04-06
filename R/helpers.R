library(plyr)
library(data.table)
library(log4r)
library(tidyr)
library(gridExtra)
library(grid)

# library(sf)
# library(maps)
# library(mapproj)
# library(mapdata)
# library(rgeos)
# library(maptools)
# library(rgdal)
# library(raster)

# number of estimated values importante ??
# allow for different models to be used.

logger <- create.logger()
logfile(logger) <- 'debug.log'
level(logger) <- 'DEBUG'

# do i need the input data frame ??...#load vs readrds
getData <- function(meas, month, year_to_start){
  if(file.exists(paste('../RData/',meas,month, year_to_start,'.RData'))){
    load(paste('../RData/',meas,month, year_to_start,'.RData'), .GlobalEnv)
    debug(logger, paste("Rdata exists"))
  } else {
    debug(logger, paste("RData does not exists"))
    # ugly...
    min_input_df_all <- load_cleaned_data(year_to_start, month, 'min_temp') #data matrix X
    output_df_all <- regression(min_input_df_all) #reg results 
    max_input_df_all <- load_cleaned_data(year_to_start, month, 'max_temp') #data matrix X
    output_df_all <- rbind(output_df_all, regression(max_input_df_all)) #reg results 
    mean_input_df_all <- load_cleaned_data(year_to_start, month, 'mean_temp') #data matrix X
    output_df_all <- rbind(output_df_all,regression(mean_input_df_all)) #reg results 
    save(output_df_all, file = paste('../RData/',meas,month, year_to_start,'.RData'))
    load(paste('../RData/',meas,month, year_to_start,'.RData'), .GlobalEnv)
  }
  return(output_df_all)
}

clean_data <- function(var, dir)
  for (i in 1:length(var)){
    df = read.delim(var[i], skip = 0, header = FALSE, as.is=TRUE, dec=".", sep = ",", na.strings=c(" ", "",'NA'), strip.white = TRUE)
    stationNum_city_prov <- paste(select(df, V1)[1,1], trimws(select(df, V2)[1,1]), province <- select(df, V3)[1,1], sep="_")
    #forward slash for precipatation files - "7025250_MONTREAL/PIERRE ELLIOTT T_QC"
    stationNum_city_prov<- str_replace_all(stationNum_city_prov, "/",'-')
    seq(from = 3, to = 35, by = 2)
    
    df <- select(df, -seq(from = 3, to = 35, by = 2))
    data <- slice(df, 5:n()) 
    (hdr <- slice(df, 3)) 
    is.na(hdr)
    
    df <- plyr::rename(data, hdr)
    #filter out -9999.9 - default values
    df <- data.frame(lapply(df, function(x){
      gsub("-9999.9", "NA", x)
    }))

    filePath= sprintf("%s_cleaned/%s.txt",dir,stationNum_city_prov)
    write.table(df, filePath, append = FALSE, sep = " ", dec = ".",
                row.names = FALSE, col.names = TRUE)
  }
# Find data files 
find_meas_data <- function(meas){
  # debug(logger, paste("|IM HERE 2|"))
  
  if(meas == 'min_temp'){
    txt_files_ls = list.files(path="../Data/Homog_monthly_min_temp_cleaned", pattern="*.txt", full.names = TRUE)
    names = list.files(path="../Data/Homog_monthly_min_temp_cleaned", pattern="*.txt")
  }
  else if(meas == 'max_temp'){
    txt_files_ls = list.files(path="../Data/Homog_monthly_max_temp_cleaned", pattern="*.txt", full.names = TRUE)
    names = list.files(path="../Data/Homog_monthly_max_temp_cleaned", pattern="*.txt")
  }
  else if(meas == 'mean_temp'){
    txt_files_ls = list.files(path="../Data/Homog_monthly_mean_temp_cleaned", pattern="*.txt", full.names = TRUE)
    names = list.files(path="../Data/Homog_monthly_mean_temp_cleaned", pattern="*.txt")
  }
  # else if(meas == 'precip'){
  #   debug(logger, paste("|IM HERE 3|"))
  #   txt_files_ls = list.files(path="../Data/Adj_monthly_total_prec_cleaned", pattern="*.txt", full.names = TRUE)
  #   names = list.files(path="../Data/Adj_monthly_total_prec_cleaned", pattern="*.txt")
  # }
  path_names <- list(txt_files_ls, names) 
  # debug(logger, paste('|FIND TEMP DATA|'))
  
  return(path_names)
}

# Load data from cleaning step
load_cleaned_data <- function(year_to_start = 1980, month = 'Feb', meas){
  # debug(logger, paste("|IM HERE 1|"))
  path_names <- find_meas_data(meas)
  txt_files_ls <- path_names[[1]]
  names <- path_names[[2]]
  ns = matrix(unlist(strsplit(names,'_',)),ncol = 3,byrow = TRUE)

  # build input data frame. 
  input_df <- data.frame()
  debug(logger, paste('|BEFORE FOR LOOP|'))
  
  for (i in 1:length(txt_files_ls)){
      nom_city <- ns[i,2]
      nom_prov <- unlist(strsplit(ns[i,3],'.txt'))
      txt_files_df <- read.table(file = txt_files_ls[i], header = TRUE, sep = " ",dec = ".", colClasses = "factor")

      years_greater<-txt_files_df[as.numeric(as.character(txt_files_df$Year))>=year_to_start,]

      y_meas <- suppressWarnings(as.numeric(as.character(unlist(years_greater[,month]))))

      x_year <- suppressWarnings(as.numeric(as.character(unlist(years_greater[,'Year']))))
      # debug(logger, paste('|START YEAR|', year_to_start, '|'))
      # debug(logger, paste('|MONTH|', x_year, '|'))
      # debug(logger, paste('|Y_MEAS|', y_meas, '|'))
      # debug(logger, paste('|X_YEAR|', x_year, '|'))
      temp_df <- data.frame(y_meas, x_year, "city" = nom_city, "prov" = nom_prov, 'meas_name' = meas)
      # debug(logger, paste('|LOAD CLEANED DATA|', 6, '|'))
      input_df <- rbind(input_df, temp_df)
  }
  # debug(logger, paste('|RETURN|'))
  
  return(input_df)
}



regression <- function(input_df){
  city_prov_vector <- unique(input_df[,c("city", 'prov')])
  city_vector <- city_prov_vector[, 'city']
  prov_vector <- city_prov_vector[, 'prov']
  meas <- unique(input_df$meas_name)
  
  output_df <- data.frame()
  for (i in 1:length(city_vector)){
    index <- which(input_df[, "city"] == city_vector[i])
    # if(numVar == 1)
    fit <- lm(y_meas[index]~x_year[index], data = input_df)
    # else if(numVar == 2)
      # fit <- lm(y_meas.x[index]~y_meas.y[index], data = input_df)
    
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
    
    curr_results_df <- data.frame("city"=city_vector[i],'prov' = prov_vector[i],  
                                  b,"r.squared"=R_2,CI_lower, CI_upper,variance,
                                  "n"=nrow(fit$model), 'meas_name' = meas,  row.names = NULL)
    output_df <- rbind(output_df,curr_results_df)
  }
  return(output_df)
}

# Interaction Model - Confirm Regression Results
# 
# city<- data.table(city_vector, stringsAsFactors = TRUE)
# fit_2 <- lm(y_temp~ city-1 + city*x_year , data = input_df)


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


# meas<- 'min_max_temp'
# month<-'Feb'
# year_to_start <- '1980'
# plot_type <- 'boxplot'
# location <- 'ON'
# loc_type <- 'prov'
# Histogram with density plot and mean line 
# just display 1980 -2017 ???
# df_consts <- data.frame(year_to_start, plot_type, location, loc_type, stringsAsFactors = FALSE)
setup_plots <- function(meas, month, df_consts){
  year_to_start <- df_consts$year_to_start
  plot_type <- df_consts$plot_type
  location <- df_consts$location
  loc_type <- df_consts$loc_type
  debug(logger, paste('-----------df_consts ----------',df_consts ))
  
  # if(!exists('output_df_all'))
    output_df_all <- getData('temp', month, year_to_start)
  if(loc_type == 'prov'){
    index <- which(output_df_all[, "prov"] == location)
    output_df_all <- output_df_all[index,]
  }
  
  p<-add_plot_data(meas, output_df_all) # returns a list plot(s)
  p<-add_plot_type(p, plot_type, loc_type) #constructs plot(s)
  # print(p)
  p<- add_plot_labels(p,month, year_to_start, location)
  grid.draw(p)
}

add_plot_type<- function(curr_plots, plot_type, loc_type){
  slope_lab <-expression(paste('Slopes (', degree, 'C)', sep = ""))
  
  for(i in 1: length(curr_plots)){
    if(plot_type == 'histogram'){
      curr_plots[[i]] <- curr_plots[[i]] + aes(x = slope)+
        geom_histogram(aes(y=..density..), colour="black", fill="white")+
        geom_density(alpha=.05, fill="#FF6666") +
        geom_vline(aes(xintercept=mean(slope)),
                   color="blue", linetype="dashed", size=1)+
        labs(y='Frequency', x = slope_lab)
        
    }
    else if(plot_type == 'boxplot'){
      x<- 'prov'
      # if(loc_type == 'prov') x<- 'city'

      curr_plots[[i]] <- curr_plots[[i]] + aes_string(x=x, y='slope')+
        geom_boxplot() +
        stat_summary(fun.y=mean, geom="point", shape=23, size=4)+
        stat_boxplot(geom = 'errorbar')+
        labs(y=slope_lab, x = 'Province')
      
    }
  }
  return(curr_plots)
    
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


add_plot_labels <-function(curr_plot, month, year_to_start, location){
  year_to_start <- toString(year_to_start)
  month <- toString(month)
  subt<- bquote(italic(.(location)*' -'~.(month) *' -' ~.(year_to_start)*' (Start Year)'))
  # title_data_val <- added to title i.e. slopes, cis, or rsquared

  # if its Min_max_temp 
  if (length(curr_plot) == 2){
    curr_plot[[1]] <- curr_plot[[1]] + 
      ggtitle('Minimum Temperature - Slopes')+
      theme(plot.title = element_text(hjust = 0.5, size = 10),
            axis.title.x = element_text(size = 9),
            axis.title.y = element_text(size = 9))
    
    curr_plot[[2]]<- curr_plot[[2]] + 
      ggtitle('Maximum Temperature - Slopes')+
      theme(plot.title = element_text(hjust = 0.5, size = 10),
            axis.title.x = element_text(size = 9),
            axis.title.y = element_text(size = 9))
    
    p<- arrangeGrob(
      top = textGrob('Min. vs Max. Temperature - Slopes',
                     gp=gpar(fontface="bold")),
      sub = textGrob(subt, gp = gpar(col = 'red', fontface='italic',
                                     fontsize = 11 )),
      curr_plot[[1]],
      curr_plot[[2]],
      bottom = textGrob(
        "this footnote is right-justified",
        gp = gpar(fontface = 3, fontsize = 9),
        hjust = 1,
        x = 1
      ),
      ncol = 1,
      heights=c(0.05, 0.5, 0.55)
    )
    return(p)
  }
  else if(length(curr_plot) == 1){
    p<- curr_plot[[1]]+
      ggtitle('Mean Temperature - Slopes')+
      labs(subtitle = subt)+
      theme(plot.title = element_text(hjust = 0.5),
            plot.subtitle = element_text(hjust = 0.5, color = 'red' ))
  }
  return(p)
}


get_city_vector <- function(prov){
  if(file.exists(paste('../RData/','constant_values','.RData'))){
    load(paste('../RData/','constant_values','.RData'), .GlobalEnv)
    city_vector <- city_prov_vector[which(city_prov_vector$prov==prov), ]
    # city_vector <- select(city_vector, city) # not working?
    city_vector <- data.frame(city_vector[, 'city'])
    city_vector$city <- as.character(city_vector$city)
    city_v <- sort(city_vector$city)
    return(city_v)
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

# library(sp)
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
# 
# newregions<-function(){
# 
#   prov <- 'ON'
#   prov_df_city_slope <- output_df_all[ which(output_df_all$prov==prov),]
#   prov_df_city_slope <- prov_df_city_slope[,c('city', 'slope')]
#   prov_df_city_slope$city <- str_to_title(prov_df_city_slope$city)
#   
#   cd <- st_read("C:/Environment_Canada_Shiny_App/Data/gcd_000b11a_e/gcd_000b11a_e.shp")
#   cd_ON <- cd[cd$PRNAME =='Ontario',]
#   cd_ON_available <- merge(cd_ON, prov_df_city_slope, by.x = 'CDNAME', by.y = 'city')
#   names(cd_ON_available)[1] <- "city"
#   
#   copy_cd_ON_available <-cd_ON_available
#   st_geometry(copy_cd_ON_available ) <- NULL
#   # ccs <- st_read("census_consolidated_subdivisions/gccs000b11a_e.shp")
#   # ccs_ON <- ccs[ccs$PRNAME =='Ontario',]
#   # ccs_ON_available <- merge(ccs_ON, prov_df_city_slope, by.x = 'CCSNAME', by.y = 'city')
#   # cd_ccs_ON_available <- merge(ccs_ON, prov_df_city_slope, by.x = 'CDNAME', by.y = 'city')
#   # names(cd_ccs_ON_available)[1] <- "city"
#   # names(ccs_ON_available)[1] <- "city"
# 
#   # st_geometry(ccs_ON_available) <- NULL
#   # st_geometry(cd_ON_available) <- NULL
#   # st_geometry(fed_ON_available) <- NULL
#   # st_geometry(cma_ON_available) <- NULL
#   # st_geometry(csd_ON_available) <- NULL
# 
#   # copy_cd_ON_available <-cd_ON_available
#   # st_geometry(copy_cd_ON_available ) <- NULL
#   # 
#   # fed <- st_read("Data/gfed000b11a_e/gfed000b11a_e.shp")
#   # fed_ON <- fed[fed$PRNAME =='Ontario',]
#   # fed_ON_available <- merge(fed_ON, prov_df_city_slope, by.x = 'CDNAME', by.y = 'city')
#   # names(fed_ON_available)[1] <- "city"
# 
#   # cma <- st_read("gcma000b11a_e/gcma000b11a_e.shp")
#   # cma_ON <- cma[cma$PRNAME =='Ontario',]
#   # cma_ON_available <- merge(cma_ON, prov_df_city_slope, by.x = 'CDNAME', by.y = 'city')
#   # names(cma_ON_available)[1] <- "city"
# 
#   # csd <- st_read("C:/Environment_Canada_Shiny_App/Data/gcsd000b11a_e/gcsd000b11a_e.shp")
#   # csd_ON <- csd[csd$PRNAME =='Ontario',]
#   # csd_ON_available <- merge(csd_ON, prov_df_city_slope, by.x = 'CSDNAME', by.y = 'city')
#   # csd_cd_ON_available <- merge(csd_ON, prov_df_city_slope, by.x = 'CDNAME', by.y = 'city')
#   # copy_csd_cd_ON_available <-csd_cd_ON_available
#   # st_geometry(copy_csd_cd_ON_available ) <- NULL
# 
#   # er <- st_read("ger_000b11a_e/ger_000b11a_e.shp")
#   # er_ON <- er[er$PRNAME =='Ontario',]
#   # er_ON_available <- merge(er_ON, prov_df_city_slope, by.x = 'ERNAME', by.y = 'city')
#   # copy_er_ON_available <-er_ON_available
#   # st_geometry(er_ON) <- NULL
# 
#   # names(csd_ON_available)[1] <- "city"
#   # copy_check7 <- check7
#   # st_geometry(copy_check7)<-NULL
# 
#   ggplot()+
#     geom_sf(data = cd_ON_available,aes(fill= slope))+
#     # geom_sf(data = fed_ON_available,aes(fill= slope))+
#     # geom_sf(data = cma_ON_available,aes(fill= slope))+
#     # geom_sf(data = csd_cd_ON_available,aes(fill= slope))+
#     # geom_sf(data = check7,aes(fill= slope))+
#     # geom_sf(data = ccs_ON_available,aes(fill= slope))+
#     # geom_sf(data= cd_ON)+
#     scale_fill_gradient(name = 'Trends',
#                         low = "blue", high = "gold2")
# 
# 
# }

# ggplot(data = check2,aes(x=long,y=lat, group = group))+
#   # geom_polygon(fill = 'grey')+
#   # geom_path(colour = "grey20", aes(group = group)) +
#   # geom_path(data= ca.cities,aes(group=group))+
#   geom_sf(aes(fill= slope))+
#   scale_fill_gradient(name = 'Trends',
#                       low = "blue", high = "gold2")