#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
minTempDir = "Homog_monthly_min_temp_cleaned"
maxTempDir = "Homog_monthly_max_temp_cleaned"
meanTempDir = "Homog_monthly_mean_temp_cleaned" 

# tempMax = list.files(path=maxTempDir, pattern="*.txt", full.names=TRUE)
# tempMin  = list.files(path=minTempDir, pattern="*.txt", full.names=TRUE)
# tempMean = list.files(path=meanTempDir, pattern="*.txt", full.names=TRUE)
library(shiny)


# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    
    # Filter data based on selections
     output$table <- DT::renderDataTable(DT::datatable({
         # Create list of text files
         txt_files_ls = list.files(path="Homog_monthly_mean_temp_cleaned", pattern="*.txt", full.names = TRUE)
         names = list.files(path="Homog_monthly_mean_temp_cleaned", pattern="*.txt")
         ns = matrix(unlist(strsplit(names,'_')),ncol = 3,byrow = TRUE)
         for (i in 1:length(txt_files_ls))
             if(ns[i,3] == "BC.txt"){
                 list = c(list, txt_files_ls[i])
             }
         
         list = list[-1]
         txt_files_df = lapply(list, function(x) {read.table(file = x, header = T, sep =" ")})
         
         # Combine them
         combined_df <- do.call("rbind", lapply(txt_files_df, as.data.frame)) 
     }))

})
