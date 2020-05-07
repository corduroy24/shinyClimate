# Module UI function
analysisUI <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)
  tagList(
    fluidRow(
      id = ns('city_eval'),
      box(
        id = ns('res_1'),
        title=uiOutput(ns('month_1')),
        solidHeader = FALSE,
        width = 6,
        background = NULL,
        status = 'warning',
        fluidRow(
          column(
            width = 6,
            em('Min'),
            uiOutput(ns('month_1_min_eval'))
          ),
          
          column(
            width = 6,
            em('Max'),
            uiOutput(ns('month_1_max_eval'))
            
          )
        )
      ),
      box(
        id = ns('res_2'),
        title=uiOutput(ns('month_2')),
        solidHeader = FALSE,
        width = 6,
        background = NULL,
        status = 'warning',
        fluidRow(
          column(
            width = 6,
            em('Min'),
            uiOutput(ns('month_2_min_eval'))
          ),
          
          column(
            width = 6,
            em('Max'),
            uiOutput(ns('month_2_max_eval'))
          )
        )
      )
    ),
    fluidRow(
      id = ns('prov_can_eval'),
      box(
        status = 'warning',
        # height = 250,
        title = uiOutput(ns('month_11')),
        
        # column(
        #   width = 6,
        #   img(src="distributions.png", width = '100%')),
        column(em('Min'),
               width = 6,
               uiOutput(ns('dist_1')),
               
               # radioButtons(ns('dist_1'), 'Type of Distribution',
               #              choices = c('Symmetric','Left-Skewed', 'Right-Skewed',
               #                          'Uniform', 'Bimodal'))
        ),
        column(em('Max'),
               width = 6,
               uiOutput(ns('dist_2'))
               # radioButtons(ns('dist_2'), 'Type of Distribution',
               #              choices = c('Symmetric','Left-Skewed', 'Right-Skewed',
               #                          'Uniform', 'Bimodal'))
        ),
        # uiOutput(ns('dist_month_1_eval')),
        
        # h4('just some text to summarize the results for the month....later')
        
      ),
      box(
        status = 'warning',
        # height = 250,
        title = uiOutput(ns('month_22')),
        
        # column(
        #   width = 6,
        #   img(src="distributions.png", width = '100%')),
        column(
          em('Min'),
          width = 6,
          uiOutput(ns('dist_11'))
          
          # radioButtons(ns('dist_choice_3'), 'Type of Distribution',
          #              choices = c('Symmetric','Left-Skewed', 'Right-Skewed',
          #                          'Uniform', 'Bimodal')),
        ),
        column(
          em('Max'),
          width = 6,
          uiOutput(ns('dist_22'))
          
          # radioButtons(ns('dist_choice_4'), 'Type of Distribution',
          #              choices = c('Symmetric','Left-Skewed', 'Right-Skewed',
          #                          'Uniform', 'Bimodal')),
        ),
        # uiOutput(ns('dist_month_2_eval')),
        
        # h4('just some text to summarize the results for the month....later')
      ),
      
    )
    # fluidRow(
    #   box(
    #     width = 12,
    #     # height = 200,
    #     status = 'warning',
    #     # h3('just some text to summarize the results for the year....later'),
    #     # uiOutput(ns('city_eval_summary'))
    #     # uiOutput(ns('prov_can_eval_summary'))
    #   )
    # )
    
  )

}


# Module server function
analysis <- function(input, output, session, sb_vars, p_vars) {
  
  observeEvent(sb_vars$region(),{
    if(sb_vars$region()  == 'City'){
      showElement('city_eval')
      hideElement('prov_can_eval')
    }
    
    else if(sb_vars$region()  == 'Province'){
      hideElement('city_eval')
      showElement('prov_can_eval')
      
    }
    else if(sb_vars$region()  == 'Canada'){
      hideElement('city_eval')
      showElement('prov_can_eval')
    }
  })
  
  observeEvent(p_vars$plot_type(),{
    if(p_vars$plot_type() == 'boxplot'){
      # hideElement('city_eval')
      hideElement('prov_can_eval')
    }
    else{
      showElement('prov_can_eval')
    }
    
  })
  
  observe({
    validate(need(sb_vars$year_to_start() != '', 'missing start year'),
             need(sb_vars$month_1() != '', 'missing month 1'),
             need(sb_vars$month_2() != '', 'missing month 2'),
             need(sb_vars$prov() != '', 'missing province'),
             need(sb_vars$city() != '', 'missing city')
    )
    
    
    df_consts <- data.frame('year_to_start' = sb_vars$year_to_start(), 
                            'prov'=sb_vars$prov(),'city' = sb_vars$city(), stringsAsFactors = FALSE )
    stats <- get_city_stats(sb_vars$month_1(), df_consts)
    # print(stats)
    min_stats <- stats[which(stats$meas_name=='min_temp'),]
    max_stats <- stats[which(stats$meas_name=='max_temp'),]
    mean_stats <- stats[which(stats$meas_name=='mean_temp'),]
    # r2_text <- HTML(paste0('R', tags$sup(2)))
    output$month_1 <- renderUI({
      # location <- paste0(sb_vars$city(),',' ,sb_vars$prov())
      # paste(sb_vars$month_1(), '-' ,location, '- (', sb_vars$year_to_start(), '- 2017)')
      sb_vars$month_1()
    })
    
    output$month_1_min_eval <- renderUI({
      res_1 <- evaluate_slope(signif(min_stats$slope,2))
      min_lower <- signif(min_stats$CI_lower,2)
      min_upper <- signif(min_stats$CI_upper,2)
      ci <- paste('(', signif(min_lower,2), ',',signif(min_upper,2),')')
      res_2 <- evaluate_ci(min_lower,min_upper)
      res_3 <- evaluate_r2(signif(min_stats_2$r.squared,2))
      boxPad(
        new_des_block_city(signif(min_stats$slope,2), res_1, 'SLOPE'),
        new_des_block_city(ci, res_2, 'CONFIDENCE INTERVAL'),
        new_des_block_city(signif(min_stats$r.squared,2), res_3, 'R\U000B2')
      )
    })
    
    output$month_1_max_eval <- renderUI({
      res_1 <- evaluate_slope(signif(max_stats$slope,2))
      max_lower <- signif(max_stats$CI_lower,2)
      max_upper <- signif(max_stats$CI_upper,2)
      ci <- paste('(', signif(max_lower,2), ',',signif(max_upper,2),')')
      res_2 <- evaluate_ci(max_lower,max_upper)
      res_3 <- evaluate_r2(signif(max_stats$r.squared,2))
      boxPad(
        new_des_block_city(signif(max_stats$slope,2), res_1, 'SLOPE'),
        new_des_block_city(ci, res_2, 'CONFIDENCE INTERVAL'), 
        new_des_block_city(signif(max_stats$r.squared,2), res_3, 'R\U000B2')
        
      )
    })
    df_consts_2 <- data.frame('year_to_start' = sb_vars$year_to_start(), 
                              'prov'=sb_vars$prov(),'city' = sb_vars$city(), stringsAsFactors = FALSE )
    stats_2 <- get_city_stats(sb_vars$month_2(), df_consts_2)
    # print(stats)
    min_stats_2 <- stats_2[which(stats_2$meas_name=='min_temp'),]
    max_stats_2 <- stats_2[which(stats_2$meas_name=='max_temp'),]
    mean_stats_2 <- stats_2[which(stats_2$meas_name=='mean_temp'),]
    
    output$month_2 <- renderUI({
      sb_vars$month_2()
    })
    
    output$month_2_min_eval <- renderUI({
      res_1_2 <- evaluate_slope(signif(min_stats_2$slope,2))
      min_lower_2 <- signif(min_stats_2$CI_lower,2)
      min_upper_2 <- signif(min_stats_2$CI_upper,2)
      ci_2 <- paste('(', signif(min_lower_2,2), ',',signif(min_upper_2,2),')')
      res_2_2 <- evaluate_ci(min_lower_2,min_upper_2)
      res_3_2 <- evaluate_r2(signif(min_stats_2$r.squared,2))
      
      boxPad(
        new_des_block_city(signif(min_stats_2$slope,2), res_1_2, 'SLOPE'),
        new_des_block_city(ci_2, res_2_2, 'CONFIDENCE INTERVAL'),
        new_des_block_city(signif(min_stats_2$r.squared,2), res_3_2, 'R\U000B2')
      )
    })
    
    output$month_2_max_eval <- renderUI({
      res_1_2 <- evaluate_slope(signif(max_stats_2$slope,2))
      max_lower_2 <- signif(max_stats_2$CI_lower,2)
      max_upper_2 <- signif(max_stats_2$CI_upper,2)
      ci_2 <- paste('(', signif(max_lower_2,2), ',',signif(max_upper_2,2),')')
      res_2_2 <- evaluate_ci(max_lower_2,max_upper_2)
      res_3_2 <- evaluate_r2(signif(max_stats_2$r.squared,2))
      boxPad(
        new_des_block_city(signif(max_stats_2$slope,2), res_1_2, 'SLOPE'),
        new_des_block_city(ci_2, res_2_2, 'CONFIDENCE INTERVAL'), 
        new_des_block_city(signif(max_stats_2$r.squared,2),res_3_2 , 'R\U000B2')
      )
    })
    
  })
  
  observe({
    validate(need(sb_vars$year_to_start() != '', 'missing start year'),
             need(sb_vars$month_1() != '', 'missing month 1'),
             need(sb_vars$month_2() != '', 'missing month 2'),
             need(sb_vars$prov() != '', 'missing province'),
             need(sb_vars$city() != '', 'missing city'),
             need(p_vars$statistic() != '', 'missing statistic')
             
    )
    output$month_11 <- renderUI({
      sb_vars$month_1()
    })
    output$month_22 <- renderUI({
      sb_vars$month_2()
    })
    
    df_consts <- data.frame('year_to_start' = sb_vars$year_to_start(), 'region' = sb_vars$region(),
                            'prov'=sb_vars$prov(),'city' = sb_vars$city(), stringsAsFactors = FALSE )
    eval_1 <- eval_hist(sb_vars$month_1(), df_consts)
    # print(eval_1)
    # print(eval_1$slope$value$max$mean)
    output$dist_1<- renderUI({
      # eval_1$slope$min
      print(p_vars$statistic())
        if(p_vars$statistic() == 'Slopes')
          boxPad(new_des_block_prov_can(eval_1$slope$value$min,eval_1$slope$min, 'SLOPE'))
        else if(p_vars$statistic() == 'Lower.Bound' | p_vars$statistic() == 'Upper.Bound'){
          boxPad(new_des_block_prov_can(eval_1$lower$value$min,eval_1$lower$min, 'Lower Bound'),
          new_des_block_prov_can(eval_1$upper$value$min, eval_1$upper$min,'Upper Bound'))
        }
        else if (p_vars$statistic() == 'R\U000B2')
          boxPad(new_des_block_prov_can(eval_1$r2$value$min, eval_1$r2$min, 'R\U000B2'))
    })
    output$dist_2<- renderUI({
      # eval_1$slope$max
        if(p_vars$statistic() == 'Slopes')
          boxPad(new_des_block_prov_can(eval_1$slope$value$max,eval_1$slope$max, 'SLOPE'))
        else if(p_vars$statistic() == 'Lower.Bound' | p_vars$statistic() == 'Upper.Bound'){
          boxPad(new_des_block_prov_can(eval_1$lower$value$max,eval_1$lower$max, 'Lower Bound'),
          new_des_block_prov_can(eval_1$upper$value$max, eval_1$upper$max,'Upper Bound'))
        }
        else if (p_vars$statistic() == 'R\U000B2')
          boxPad(new_des_block_prov_can(eval_1$r2$value$max, eval_1$r2$max , 'R\U000B2'))
    })
    
    eval_2 <- eval_hist(sb_vars$month_2(), df_consts)
    
    output$dist_11<- renderUI({
      # eval_2$slope$min
        if(p_vars$statistic() == 'Slopes')
          boxPad(new_des_block_prov_can(eval_2$slope$value$min,eval_2$slope$min, 'SLOPE'))
        else if(p_vars$statistic() == 'Lower.Bound' | p_vars$statistic() == 'Upper.Bound')({
          boxPad(new_des_block_prov_can(eval_2$lower$value$min,eval_2$lower$min, 'Lower Bound'),
          new_des_block_prov_can(eval_2$upper$value$min, eval_2$upper$min,'Upper Bound'))
        })
        else if (p_vars$statistic() == 'R\U000B2')
          boxPad(new_des_block_prov_can(eval_2$r2$value$min, eval_2$r2$min , 'R\U000B2'))
    })
    output$dist_22<- renderUI({
      # eval_2$slope$max
        if(p_vars$statistic() == 'Slopes')
          boxPad(new_des_block_prov_can(eval_2$slope$value$max,eval_2$slope$max, 'SLOPE'))
        else if(p_vars$statistic() == 'Lower.Bound' || p_vars$statistic() == 'Upper.Bound'){
          boxPad(new_des_block_prov_can(eval_2$lower$value$max,eval_2$lower$max, 'Lower Bound'),
          new_des_block_prov_can(eval_2$upper$value$max, eval_2$upper$max,'Upper Bound')) 
        }
        else if (p_vars$statistic() == 'R\U000B2')
          boxPad(new_des_block_prov_can(eval_2$r2$value$max, eval_2$r2$max , 'R\U000B2'))
    })
    
  })

}



