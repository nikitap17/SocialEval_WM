meanp_size = 1.5
p_size = 1
raincloud_prepare_data <- function(data, id_variable, y_variable, within_factor, between_factor, within_levels, between_levels, jit_distance = 0.09, jit_seed = 2){
  d <- data.frame("id" = data[,id_variable],
                  "y_axis" = data[, y_variable],
                  "within_factor" = data[, within_factor],
                  "between_factor" = data[, between_factor])
  colnames(d) <- c("id", "y_axis", "within_factor", "between_factor")
  
  within_uniques <- within_levels
  between_uniques <- between_levels
  
  if (length(between_uniques) == 4){
    d$x_axis <- case_when(d$within_factor == within_uniques[1] & d$between_factor == between_uniques[1] ~ 1,
                          d$within_factor == within_uniques[2] & d$between_factor == between_uniques[1] ~ 2,
                          d$within_factor == within_uniques[1] & d$between_factor == between_uniques[2] ~ 3,
                          d$within_factor == within_uniques[2] & d$between_factor == between_uniques[2] ~ 4,
                          d$within_factor == within_uniques[1] & d$between_factor == between_uniques[3] ~ 5,
                          d$within_factor == within_uniques[2] & d$between_factor == between_uniques[3] ~ 6,
                          d$within_factor == within_uniques[1] & d$between_factor == between_uniques[4] ~ 7,
                          d$within_factor == within_uniques[2] & d$between_factor == between_uniques[4] ~ 8)
    
    condition_dictionary <- list("1" = c(within_uniques[1], between_uniques[1]),
                                 "2" = c(within_uniques[2], between_uniques[1]),
                                 "3" = c(within_uniques[1], between_uniques[2]),
                                 "4" = c(within_uniques[2], between_uniques[2]),
                                 "5" = c(within_uniques[1], between_uniques[3]),
                                 "6" = c(within_uniques[2], between_uniques[3]),
                                 "7" = c(within_uniques[1], between_uniques[4]),
                                 "8" = c(within_uniques[2], between_uniques[4]))
    
  } else if (length(between_uniques) == 3){
    d$x_axis <- case_when(d$within_factor == within_uniques[1] & d$between_factor == between_uniques[1] ~ 1,
                          d$within_factor == within_uniques[2] & d$between_factor == between_uniques[1] ~ 2,
                          d$within_factor == within_uniques[1] & d$between_factor == between_uniques[2] ~ 3,
                          d$within_factor == within_uniques[2] & d$between_factor == between_uniques[2] ~ 4,
                          d$within_factor == within_uniques[1] & d$between_factor == between_uniques[3] ~ 5,
                          d$within_factor == within_uniques[2] & d$between_factor == between_uniques[3] ~ 6)
    
    condition_dictionary <- list("1" = c(within_uniques[1], between_uniques[1]),
                                 "2" = c(within_uniques[2], between_uniques[1]),
                                 "3" = c(within_uniques[1], between_uniques[2]),
                                 "4" = c(within_uniques[2], between_uniques[2]),
                                 "5" = c(within_uniques[1], between_uniques[3]),
                                 "6" = c(within_uniques[2], between_uniques[3]))
    
  } else if (length(between_uniques) == 2){
    d$x_axis <- case_when(d$within_factor == within_uniques[1] & d$between_factor == between_uniques[1] ~ 1,
                          d$within_factor == within_uniques[2] & d$between_factor == between_uniques[1] ~ 2,
                          d$within_factor == within_uniques[1] & d$between_factor == between_uniques[2] ~ 3,
                          d$within_factor == within_uniques[2] & d$between_factor == between_uniques[2] ~ 4)
    
    condition_dictionary <- list("1" = c(within_uniques[1], between_uniques[1]),
                                 "2" = c(within_uniques[2], between_uniques[1]),
                                 "3" = c(within_uniques[1], between_uniques[2]),
                                 "4" = c(within_uniques[2], between_uniques[2]))
  }
  
  set.seed(jit_seed)
  d$jit <- jitter(d$x_axis, amount = jit_distance)
  
  return(list(data = d, conditions = condition_dictionary))
}


raincloud_plot <- function(data, # data as preprocessed with raincloud_prepare_data
                            within_levels, # labels of within condition as they appear in the data
                            within_label = "Time", # name of the within factor
                            between_levels, # labels of between condition as they appear in the data
                            boxplot = TRUE, # optional for a 2x2 design - violins must be TRUE
                            violins = TRUE, # plot violins or not
                            
                            # plot the between levels labels above or below the plots.
                            # Allowed arguments = c("top", "bottom")
                            # If the labels are plotted below, they will appear as x-ticks and will
                            # thus replace the within level annotations. These will instead appear
                            # in the legend
                            between_annotations = "top", 
                            colors = c('dodgerblue','darkorange')) {
  
  
  # DESCRIPTION
  # Creating raincloud plots for up to three conditions.
  
  
  # Create summary dataframe
  summary_df <- data %>%
    group_by(x_axis) %>%
    dplyr::summarise(score_mean = mean(y_axis, na.rm = TRUE),
                     sd = sd(y_axis, na.rm = TRUE), N = n()) %>%
    mutate(se = sd / sqrt(N),
           ci = qt(1 - (0.05 / 2), N - 1) * se)
  summary_df$group <- summary_df$x_axis
  summary_df <- summary_df[order(summary_df$group),]
  
  
  ### violins = FALSE
  if (violins == FALSE){
    if (length(unique(data$between_factor)) == 2) {
      summary_df_l <- summary_df %>% filter(group == "1" | group == "2")
      summary_df_h <- summary_df %>% filter(group == "3" | group == "4")
      
      x_tick_means_1 <- c(.87, 2.15)
      x_tick_means_2 <- c(2.87, 4.15)
      
      raincloud <- ggplot(data = data, aes(y = y_axis)) +
        geom_point(shape = 16, data = data %>% filter(x_axis %in% c("1","2")), aes(x = jit, color = as.factor(x_axis)), size = p_size, alpha = .6) +
        geom_point(shape = 16, data = data %>% filter(x_axis == "3"), aes(x = jit), color = colors[1], size = p_size, alpha = .6) +
        geom_point(shape = 16, data = data %>% filter(x_axis == "4"), aes(x = jit), color = colors[2], size = p_size, alpha = .6) +
        
        geom_line(data = data %>% filter(x_axis %in% c("1", "2")), aes(x = jit, group = id), color = 'lightgray', alpha = .3) +
        geom_line(data = data %>% filter(x_axis %in% c("3", "4")), aes(x = jit, group = id), color = 'lightgray', alpha = .3) +
        
        geom_line(data = summary_df_l, aes(x = x_tick_means_1, y = score_mean), color = 'gray', linewidth = .8) +
        geom_line(data = summary_df_h, aes(x = x_tick_means_2, y = score_mean), color = 'gray', linewidth = .8) +
        
        geom_point(shape = 16, data = data %>% filter(x_axis == "1"), aes(x = x_axis, y = summary_df$score_mean[1]), position = position_nudge(x = -.15), color = colors[1], alpha = .6, size = meanp_size) +
        geom_errorbar(data = data %>% filter(x_axis == "1"), aes(x = x_axis, y = summary_df$score_mean[1], ymin = summary_df$score_mean[1] - summary_df$ci[1], ymax = summary_df$score_mean[1] + summary_df$ci[1]), position = position_nudge(-.15), color = colors[1], width = 0.05, linewidth = 0.4, alpha = .6) +
        geom_point(shape = 16, data = data %>% filter(x_axis == "2"), aes(x = x_axis, y = summary_df$score_mean[2]), position = position_nudge(x = .15), color = colors[2], alpha = .6, size = meanp_size) +
        geom_errorbar(data = data %>% filter(x_axis == "2"), aes(x = x_axis, y = summary_df$score_mean[2], ymin = summary_df$score_mean[2] - summary_df$ci[2], ymax = summary_df$score_mean[2] + summary_df$ci[2]), position = position_nudge(x = .15), color = colors[2], width = 0.05, linewidth = 0.4, alpha = .6) +
        geom_point(shape = 16, data = data %>% filter(x_axis == "3"), aes(x = x_axis, y = summary_df$score_mean[3]), position = position_nudge(x = -.15), color = colors[1], alpha = .6, size = meanp_size) +
        geom_errorbar(data = data %>% filter(x_axis == "3"), aes(x = x_axis, y = summary_df$score_mean[3], ymin = summary_df$score_mean[3] - summary_df$ci[3], ymax = summary_df$score_mean[3] + summary_df$ci[3]), position = position_nudge(-.15), color = colors[1], width = 0.05, linewidth = 0.4, alpha = .5) +
        geom_point(shape = 16, data = data %>% filter(x_axis == "4"), aes(x = x_axis, y = summary_df$score_mean[4]), position = position_nudge(x = .15), color = colors[2], alpha = .6, size = meanp_size) +
        geom_errorbar(data = data %>% filter(x_axis == "4"), aes(x = x_axis, y = summary_df$score_mean[4], ymin = summary_df$score_mean[4] - summary_df$ci[4], ymax = summary_df$score_mean[4] + summary_df$ci[4]), position = position_nudge(.15), color = colors[2], width = 0.05, linewidth = 0.4, alpha = .5) +
        
        
      # Additional settings
        scale_color_manual(values = colors, labels = within_levels, name = within_label) +
        guides(color = guide_legend(override.aes = list(shape = 15, size = 4)))

      
      if (between_annotations == "top"){
        raincloud <- raincloud +
        scale_x_continuous(breaks = c(1, 2, 3, 4), labels = rep(within_levels, length(unique(data$between_factor))), limits = c(0.5, 4.5)) +
          annotate("text", x = 1.5, y = max(data$y_axis) * 1.05, label = between_levels[1], hjust = 0.5) +
          annotate("text", x = 3.5, y = max(data$y_axis) * 1.05, label = between_levels[2], hjust = 0.5) +
          theme_classic() +
          theme(legend.position = "none",
                axis.title.x = element_text(vjust = -3),
                axis.title.y = element_text(vjust = 3),
                plot.margin = margin(b = 15, l = 15))

      } else if (between_annotations == "bottom"){
        raincloud <- raincloud +
        scale_x_continuous(breaks = c(1.5, 3.5), labels = between_levels, limits = c(0.5, 4.5)) +
          theme_classic() +
          # scale_color_manual(values = colors,
          #                    labels = within_levels) +
          # guides(color = guide_legend(override.aes = list(linetype = 0, shape = 15))) +

          theme(legend.position = "top",
                axis.title.x = element_text(vjust = -3),
                axis.title.y = element_text(vjust = 3),
                plot.margin = margin(b = 15, l = 15))
      }
      

      
      return(raincloud)
      
      
    } else if (length(unique(data$between_factor)) == 3) {
      summary_df_l <- summary_df %>% filter(group == "1" | group == "2")
      summary_df_h <- summary_df %>% filter(group == "3" | group == "4")
      summary_df_3 <- summary_df %>% filter(group == "5" | group == "6")
      
      dodge1 = 0
      dodge2 = -0.25
      dodge = -0.5
      
      x_tick_means_1 <- c(.87 + dodge1, 2.15 + dodge1)
      x_tick_means_2 <- c(2.87 + dodge2, 4.15 + dodge2)
      x_tick_means_3 <- c(4.87 + dodge, 6.15 + dodge)
      
      raincloud <- ggplot(data = data, aes(y = y_axis)) +
        geom_point(shape = 16, data = data %>% filter(x_axis %in% c("1","2")), aes(x = jit, color = as.factor(x_axis)), size = p_size, alpha = .6) +
        geom_point(shape = 16, data = data %>% filter(x_axis == "3"), aes(x = jit+dodge2), color = colors[1], size = p_size, alpha = .6) +
        geom_point(shape = 16, data = data %>% filter(x_axis == "4"), aes(x = jit+dodge2), color = colors[2], size = p_size, alpha = .6) +
        geom_point(shape = 16, data = data %>% filter(x_axis == "5"), aes(x = jit+dodge), color = colors[1], size = p_size, alpha = .6) +
        geom_point(shape = 16, data = data %>% filter(x_axis == "6"), aes(x = jit+dodge), color = colors[2], size = p_size, alpha = .6) +
        
        geom_line(data = data %>% filter(x_axis %in% c("1", "2")), aes(x = jit+dodge1, group = id), color = 'lightgray', alpha = .3) +
        geom_line(data = data %>% filter(x_axis %in% c("3", "4")), aes(x = jit+dodge2, group = id), color = 'lightgray', alpha = .3) +
        geom_line(data = data %>% filter(x_axis %in% c("5", "6")), aes(x = jit + dodge, group = id), color = 'lightgray', alpha = .3) +
        
        geom_line(data = summary_df_l, aes(x = x_tick_means_1, y = score_mean), color = 'gray', linewidth = .8) +
        geom_line(data = summary_df_h, aes(x = x_tick_means_2, y = score_mean), color = 'gray', linewidth = .8) +
        geom_line(data = summary_df_3, aes(x = x_tick_means_3, y = score_mean), color = 'gray', linewidth = .8) +
        
        geom_point(shape = 16, data = data %>% filter(x_axis == "1"), aes(x = x_axis+dodge1, y = summary_df$score_mean[1]), position = position_nudge(x = -.15), color = colors[1], alpha = .6, size = meanp_size) +
        geom_errorbar(data = data %>% filter(x_axis == "1"), aes(x = x_axis+dodge1, y = summary_df$score_mean[1], ymin = summary_df$score_mean[1] - summary_df$ci[1], ymax = summary_df$score_mean[1] + summary_df$ci[1]), position = position_nudge(-.15), color = colors[1], width = 0.05, linewidth = 0.4, alpha = .6) +
        geom_point(shape = 16, data = data %>% filter(x_axis == "2"), aes(x = x_axis+dodge1, y = summary_df$score_mean[2]), position = position_nudge(x = .15), color = colors[2], alpha = .6, size = meanp_size) +
        geom_errorbar(data = data %>% filter(x_axis == "2"), aes(x = x_axis+dodge1, y = summary_df$score_mean[2], ymin = summary_df$score_mean[2] - summary_df$ci[2], ymax = summary_df$score_mean[2] + summary_df$ci[2]), position = position_nudge(x = .15), color = colors[2], width = 0.05, linewidth = 0.4, alpha = .6) +
        geom_point(shape = 16, data = data %>% filter(x_axis == "3"), aes(x = x_axis+dodge2, y = summary_df$score_mean[3]), position = position_nudge(x = -.15), color = colors[1], alpha = .6, size = meanp_size) +
        geom_errorbar(data = data %>% filter(x_axis == "3"), aes(x = x_axis+dodge2, y = summary_df$score_mean[3], ymin = summary_df$score_mean[3] - summary_df$ci[3], ymax = summary_df$score_mean[3] + summary_df$ci[3]), position = position_nudge(-.15), color = colors[1], width = 0.05, linewidth = 0.4, alpha = .5) +
        geom_point(shape = 16, data = data %>% filter(x_axis == "4"), aes(x = x_axis+dodge2, y = summary_df$score_mean[4]), position = position_nudge(x = .15), color = colors[2], alpha = .6, size = meanp_size) +
        geom_errorbar(data = data %>% filter(x_axis == "4"), aes(x = x_axis+dodge2, y = summary_df$score_mean[4], ymin = summary_df$score_mean[4] - summary_df$ci[4], ymax = summary_df$score_mean[4] + summary_df$ci[4]), position = position_nudge(.15), color = colors[2], width = 0.05, linewidth = 0.4, alpha = .5) +
        geom_point(shape = 16, data = data %>% filter(x_axis == "5"), aes(x = x_axis + dodge, y = summary_df$score_mean[5]), position = position_nudge(x = -.15), color = colors[1], alpha = .6, size = meanp_size) +
        geom_errorbar(data = data %>% filter(x_axis == "5"), aes(x = x_axis + dodge, y = summary_df$score_mean[5], ymin = summary_df$score_mean[5] - summary_df$ci[5], ymax = summary_df$score_mean[5] + summary_df$ci[5]), position = position_nudge(-.15), color = colors[1], width = 0.05, linewidth = 0.4, alpha = .5) +
        geom_point(shape = 16, data = data %>% filter(x_axis == "6"), aes(x = x_axis + dodge, y = summary_df$score_mean[6]), position = position_nudge(x = .15), color = colors[2], alpha = .6, size = meanp_size) +
        geom_errorbar(data = data %>% filter(x_axis == "6"), aes(x = x_axis + dodge, y = summary_df$score_mean[6], ymin = summary_df$score_mean[6] - summary_df$ci[6], ymax = summary_df$score_mean[6] + summary_df$ci[6]), position = position_nudge(.15), color = colors[2], width = 0.05, linewidth = 0.4, alpha = .5) +
        
        
        # Additional settings
        scale_color_manual(values = colors, labels = within_levels, name = within_label) +
        guides(color = guide_legend(override.aes = list(shape = 15, size = 4)))
      
      
      if (between_annotations == "top"){
        raincloud <- raincloud +
          scale_x_continuous(breaks = c(1+dodge1, 2+dodge1, 3+dodge2, 4+dodge2, 5 + dodge, 6 + dodge), labels = rep(within_levels, length(unique(data$between_factor))), limits = c(0.5, 6.5+dodge)) +
          annotate("text", x = 1.5+dodge1, y = max(data$y_axis) * 1.05, label = between_levels[1], hjust = 0.5) +
          annotate("text", x = 3.5+dodge2, y = max(data$y_axis) * 1.05, label = between_levels[2], hjust = 0.5) +
          annotate("text", x = 5.5+dodge, y = max(data$y_axis) * 1.05, label = between_levels[3], hjust = 0.5) +
          theme_classic() +
          theme(legend.position = "none",
                axis.title.x = element_text(vjust = -3),
                axis.title.y = element_text(vjust = 3),
                plot.margin = margin(b = 15, l = 15))
        
      } else if (between_annotations == "bottom"){
        raincloud <- raincloud +
          scale_x_continuous(breaks = c(1.5+dodge1, 3.5+dodge2, 5.5+dodge), labels = between_levels, limits = c(0.5, 6.5+dodge)) +
          theme_classic() +
          theme(legend.position = "top",
                axis.title.x = element_text(vjust = -3),
                axis.title.y = element_text(vjust = 3),
                plot.margin = margin(b = 15, l = 15))
      }
      
      return(raincloud)
      
      
    } else if (length(unique(data$between_factor)) == 4) {
      summary_df_l <- summary_df %>% filter(group == "1" | group == "2")
      summary_df_h <- summary_df %>% filter(group == "3" | group == "4")
      summary_df_3 <- summary_df %>% filter(group == "5" | group == "6")
      summary_df_4 <- summary_df %>% filter(group == "7" | group == "8")
      
      dodge1 = 0
      dodge2 = -0.25
      dodge = -0.5
      dodge4 = -0.75
      
      x_tick_means_1 <- c(.87 + dodge1, 2.15 + dodge1)
      x_tick_means_2 <- c(2.87 + dodge2, 4.15 + dodge2)
      x_tick_means_3 <- c(4.87 + dodge, 6.15 + dodge)
      x_tick_means_4 <- c(6.87 + dodge4, 8.15 + dodge4)
      
      
      raincloud <- ggplot(data = data, aes(y = y_axis)) +
        geom_point(shape = 16, data = data %>% filter(x_axis %in% c("1","2")), aes(x = jit, color = as.factor(x_axis)), size = p_size, alpha = .6) +
        geom_point(shape = 16, data = data %>% filter(x_axis == "3"), aes(x = jit+dodge2), color = colors[1], size = p_size, alpha = .6) +
        geom_point(shape = 16, data = data %>% filter(x_axis == "4"), aes(x = jit+dodge2), color = colors[2], size = p_size, alpha = .6) +
        geom_point(shape = 16, data = data %>% filter(x_axis == "5"), aes(x = jit+dodge), color = colors[1], size = p_size, alpha = .6) +
        geom_point(shape = 16, data = data %>% filter(x_axis == "6"), aes(x = jit+dodge), color = colors[2], size = p_size, alpha = .6) +
        geom_point(shape = 16, data = data %>% filter(x_axis == "7"), aes(x = jit+dodge4), color = colors[1], size = p_size, alpha = .6) +
        geom_point(shape = 16, data = data %>% filter(x_axis == "8"), aes(x = jit+dodge4), color = colors[2], size = p_size, alpha = .6) +
        
        geom_line(data = data %>% filter(x_axis %in% c("1", "2")), aes(x = jit+dodge1, group = id), color = 'lightgray', alpha = .3) +
        geom_line(data = data %>% filter(x_axis %in% c("3", "4")), aes(x = jit+dodge2, group = id), color = 'lightgray', alpha = .3) +
        geom_line(data = data %>% filter(x_axis %in% c("5", "6")), aes(x = jit + dodge, group = id), color = 'lightgray', alpha = .3) +
        geom_line(data = data %>% filter(x_axis %in% c("7", "8")), aes(x = jit + dodge4, group = id), color = 'lightgray', alpha = .3) +
        
        geom_line(data = summary_df_l, aes(x = x_tick_means_1, y = score_mean), color = 'gray', linewidth = .8) +
        geom_line(data = summary_df_h, aes(x = x_tick_means_2, y = score_mean), color = 'gray', linewidth = .8) +
        geom_line(data = summary_df_3, aes(x = x_tick_means_3, y = score_mean), color = 'gray', linewidth = .8) +
        geom_line(data = summary_df_4, aes(x = x_tick_means_4, y = score_mean), color = 'gray', linewidth = .8) +
        
        geom_point(shape = 16, data = data %>% filter(x_axis == "1"), aes(x = x_axis+dodge1, y = summary_df$score_mean[1]), position = position_nudge(x = -.15), color = colors[1], alpha = .6, size = meanp_size) +
        geom_errorbar(data = data %>% filter(x_axis == "1"), aes(x = x_axis+dodge1, y = summary_df$score_mean[1], ymin = summary_df$score_mean[1] - summary_df$ci[1], ymax = summary_df$score_mean[1] + summary_df$ci[1]), position = position_nudge(-.15), color = colors[1], width = 0.05, linewidth = 0.4, alpha = .6) +
        geom_point(shape = 16, data = data %>% filter(x_axis == "2"), aes(x = x_axis+dodge1, y = summary_df$score_mean[2]), position = position_nudge(x = .15), color = colors[2], alpha = .6, size = meanp_size) +
        geom_errorbar(data = data %>% filter(x_axis == "2"), aes(x = x_axis+dodge1, y = summary_df$score_mean[2], ymin = summary_df$score_mean[2] - summary_df$ci[2], ymax = summary_df$score_mean[2] + summary_df$ci[2]), position = position_nudge(x = .15), color = colors[2], width = 0.05, linewidth = 0.4, alpha = .6) +
        geom_point(shape = 16, data = data %>% filter(x_axis == "3"), aes(x = x_axis+dodge2, y = summary_df$score_mean[3]), position = position_nudge(x = -.15), color = colors[1], alpha = .6, size = meanp_size) +
        geom_errorbar(data = data %>% filter(x_axis == "3"), aes(x = x_axis+dodge2, y = summary_df$score_mean[3], ymin = summary_df$score_mean[3] - summary_df$ci[3], ymax = summary_df$score_mean[3] + summary_df$ci[3]), position = position_nudge(-.15), color = colors[1], width = 0.05, linewidth = 0.4, alpha = .6) +
        geom_point(shape = 16, data = data %>% filter(x_axis == "4"), aes(x = x_axis+dodge2, y = summary_df$score_mean[4]), position = position_nudge(x = .15), color = colors[2], alpha = .6, size = meanp_size) +
        geom_errorbar(data = data %>% filter(x_axis == "4"), aes(x = x_axis+dodge2, y = summary_df$score_mean[4], ymin = summary_df$score_mean[4] - summary_df$ci[4], ymax = summary_df$score_mean[4] + summary_df$ci[4]), position = position_nudge(.15), color = colors[2], width = 0.05, linewidth = 0.4, alpha = .6) +
        geom_point(shape = 16, data = data %>% filter(x_axis == "5"), aes(x = x_axis + dodge, y = summary_df$score_mean[5]), position = position_nudge(x = -.15), color = colors[1], alpha = .6, size = meanp_size) +
        geom_errorbar(data = data %>% filter(x_axis == "5"), aes(x = x_axis + dodge, y = summary_df$score_mean[5], ymin = summary_df$score_mean[5] - summary_df$ci[5], ymax = summary_df$score_mean[5] + summary_df$ci[5]), position = position_nudge(-.15), color = colors[1], width = 0.05, linewidth = 0.4, alpha = .6) +
        geom_point(shape = 16, data = data %>% filter(x_axis == "6"), aes(x = x_axis + dodge, y = summary_df$score_mean[6]), position = position_nudge(x = .15), color = colors[2], alpha = .6, size = meanp_size) +
        geom_errorbar(data = data %>% filter(x_axis == "6"), aes(x = x_axis + dodge, y = summary_df$score_mean[6], ymin = summary_df$score_mean[6] - summary_df$ci[6], ymax = summary_df$score_mean[6] + summary_df$ci[6]), position = position_nudge(.15), color = colors[2], width = 0.05, linewidth = 0.4, alpha = .6) +
        geom_point(shape = 16, data = data %>% filter(x_axis == "7"), aes(x = x_axis + dodge4, y = summary_df$score_mean[7]), position = position_nudge(x = -.15), color = colors[1], alpha = .6, size = meanp_size) +
        geom_errorbar(data = data %>% filter(x_axis == "7"), aes(x = x_axis + dodge4, y = summary_df$score_mean[7], ymin = summary_df$score_mean[7] - summary_df$ci[7], ymax = summary_df$score_mean[7] + summary_df$ci[7]), position = position_nudge(-.15), color = colors[1], width = 0.05, linewidth = 0.4, alpha = .6) +
        geom_point(shape = 16, data = data %>% filter(x_axis == "8"), aes(x = x_axis + dodge4, y = summary_df$score_mean[8]), position = position_nudge(x = .15), color = colors[2], alpha = .6, size = meanp_size) +
        geom_errorbar(data = data %>% filter(x_axis == "8"), aes(x = x_axis + dodge4, y = summary_df$score_mean[8], ymin = summary_df$score_mean[8] - summary_df$ci[8], ymax = summary_df$score_mean[8] + summary_df$ci[8]), position = position_nudge(.15), color = colors[2], width = 0.05, linewidth = 0.4, alpha = .6) +
        
        
        # Additional settings
        scale_color_manual(values = colors, labels = within_levels, name = within_label) +
        guides(color = guide_legend(override.aes = list(shape = 15, size = 4)))
      
      
      if (between_annotations == "top"){
        raincloud <- raincloud +
          scale_x_continuous(breaks = c(1+dodge1, 2+dodge1, 3+dodge2, 4+dodge2, 5 + dodge, 6 + dodge, 7 + dodge4, 8 + dodge4), labels = rep(within_levels, length(unique(data$between_factor))), limits = c(0.5, 8.5+ dodge4)) +
          annotate("text", x = 1.5+dodge1, y = max(data$y_axis) * 1.05, label = between_levels[1], hjust = 0.5) +
          annotate("text", x = 3.5+dodge2, y = max(data$y_axis) * 1.05, label = between_levels[2], hjust = 0.5) +
          annotate("text", x = 5.5+dodge, y = max(data$y_axis) * 1.05, label = between_levels[3], hjust = 0.5) +
          annotate("text", x = 7.5+dodge4, y = max(data$y_axis) * 1.05, label = between_levels[4], hjust = 0.5) +
          theme_classic() +
          theme(legend.position = "none",
                axis.title.x = element_text(vjust = -3),
                axis.title.y = element_text(vjust = 3),
                plot.margin = margin(b = 15, l = 15))
        
      } else if (between_annotations == "bottom"){
        raincloud <- raincloud +
          scale_x_continuous(breaks = c(1.5+dodge1, 3.5+dodge2, 5.5+dodge, 7.5+dodge4), labels = between_levels, limits = c(0.5, 8.5+dodge)) +
          theme_classic() +
          theme(legend.position = "top",
                axis.title.x = element_text(vjust = -3),
                axis.title.y = element_text(vjust = 3),
                plot.margin = margin(b = 15, l = 15))
      }
      
      return(raincloud)
    }
    
    
    ### violins = TRUE
  } else{
    
    if (length(unique(data$between_factor)) == 2) {
      summary_df_l <- summary_df %>% filter(group == "1" | group == "2")
      summary_df_h <- summary_df %>% filter(group == "3" | group == "4")
      
      x_tick_means_1 <- c(.87, 2.15)
      x_tick_means_2 <- c(2.87, 4.15)
      
      raincloud <- ggplot(data = data, aes(y = y_axis)) +
        geom_point(shape = 16, data = data %>% filter(x_axis %in% c("1","2")), aes(x = jit, color = as.factor(x_axis)), size = p_size, alpha = .6) +
        geom_point(shape = 16, data = data %>% filter(x_axis == "3"), aes(x = jit), color = colors[1], size = p_size, alpha = .6) +
        geom_point(shape = 16, data = data %>% filter(x_axis == "4"), aes(x = jit), color = colors[2], size = p_size, alpha = .6) +
        
        geom_line(data = data %>% filter(x_axis %in% c("1", "2")), aes(x = jit, group = id), color = 'lightgray', alpha = .3) +
        geom_line(data = data %>% filter(x_axis %in% c("3", "4")), aes(x = jit, group = id), color = 'lightgray', alpha = .3) +
        
        # geom_half_violin(data = data %>% filter(x_axis == "1"), aes(x = x_axis, y = y_axis), position = position_nudge(x = -.24), side = "l", fill = colors[1], alpha = .6) +
        # geom_half_violin(data = data %>% filter(x_axis == "2"), aes(x = x_axis, y = y_axis), position = position_nudge(x = -1.24), side = "l", fill = colors[2], alpha = .6) +
        # geom_half_violin(data = data %>% filter(x_axis == "3"), aes(x = x_axis, y = y_axis), position = position_nudge(x = 1.24), side = "r", fill = colors[1], alpha = .6) +
        # geom_half_violin(data = data %>% filter(x_axis == "4"), aes(x = x_axis, y = y_axis), position = position_nudge(x = .24), side = "r", fill = colors[2], alpha = .6) +
        # 
        geom_line(data = summary_df_l, aes(x = x_tick_means_1, y = score_mean), color = 'gray', linewidth = .8) +
        geom_line(data = summary_df_h, aes(x = x_tick_means_2, y = score_mean), color = 'gray', linewidth = .8) +
        
        geom_point(shape = 16, data = data %>% filter(x_axis == "1"), aes(x = x_axis, y = summary_df$score_mean[1]), position = position_nudge(x = -.15), color = colors[1], alpha = .6, size = meanp_size) +
        geom_errorbar(data = data %>% filter(x_axis == "1"), aes(x = x_axis, y = summary_df$score_mean[1], ymin = summary_df$score_mean[1] - summary_df$ci[1], ymax = summary_df$score_mean[1] + summary_df$ci[1]), position = position_nudge(-.15), color = colors[1], width = 0.05, linewidth = 0.4, alpha = .6) +
        geom_point(shape = 16, data = data %>% filter(x_axis == "2"), aes(x = x_axis, y = summary_df$score_mean[2]), position = position_nudge(x = .15), color = colors[2], alpha = .6, size = meanp_size) +
        geom_errorbar(data = data %>% filter(x_axis == "2"), aes(x = x_axis, y = summary_df$score_mean[2], ymin = summary_df$score_mean[2] - summary_df$ci[2], ymax = summary_df$score_mean[2] + summary_df$ci[2]), position = position_nudge(x = .15), color = colors[2], width = 0.05, linewidth = 0.4, alpha = .6) +
        geom_point(shape = 16, data = data %>% filter(x_axis == "3"), aes(x = x_axis, y = summary_df$score_mean[3]), position = position_nudge(x = -.15), color = colors[1], alpha = .6, size = meanp_size) +
        geom_errorbar(data = data %>% filter(x_axis == "3"), aes(x = x_axis, y = summary_df$score_mean[3], ymin = summary_df$score_mean[3] - summary_df$ci[3], ymax = summary_df$score_mean[3] + summary_df$ci[3]), position = position_nudge(-.15), color = colors[1], width = 0.05, linewidth = 0.4, alpha = .6) +
        geom_point(shape = 16, data = data %>% filter(x_axis == "4"), aes(x = x_axis, y = summary_df$score_mean[4]), position = position_nudge(x = .15), color = colors[2], alpha = .6, size = meanp_size) +
        geom_errorbar(data = data %>% filter(x_axis == "4"), aes(x = x_axis, y = summary_df$score_mean[4], ymin = summary_df$score_mean[4] - summary_df$ci[4], ymax = summary_df$score_mean[4] + summary_df$ci[4]), position = position_nudge(.15), color = colors[2], width = 0.05, linewidth = 0.4, alpha = .6)
      
      
      if (boxplot){
        raincloud <- raincloud +
          
          geom_half_boxplot(
            data = data %>% filter(x_axis=="1"), aes(x=x_axis, y = y_axis), position = position_nudge(x = -.3),
            side = "r",outlier.shape = NA, center = TRUE, errorbar.draw = FALSE, width = .2,
            fill = colors[1], alpha = .6) +
          geom_half_boxplot(
            data = data %>% filter(x_axis=="2"), aes(x=x_axis, y = y_axis), position = position_nudge(x = -1.3),
            side = "l",outlier.shape = NA, center = TRUE, errorbar.draw = FALSE, width = .2,
            fill = colors[2], alpha = .6) +
          geom_half_boxplot(
            data = data %>% filter(x_axis=="3"), aes(x=x_axis, y = y_axis), position = position_nudge(x = 1.3),
            side = "r",outlier.shape = NA, center = TRUE, errorbar.draw = FALSE, width = .2,
            fill = colors[1], alpha = .6) +
          geom_half_boxplot(
            data = data %>% filter(x_axis=="4"), aes(x=x_axis, y = y_axis), position = position_nudge(x = .2),
            side = "r",outlier.shape = NA, center = TRUE, errorbar.draw = FALSE, width = .2,
            fill = colors[2], alpha = .6) +
          
          geom_half_violin(
            data = data %>% filter(x_axis=="1"),aes(x = x_axis, y = y_axis), position = position_nudge(x = -.45),
            side = "l", fill = colors[1], alpha = .6) +
          geom_half_violin(
            data = data %>% filter(x_axis=="2"),aes(x = x_axis, y = y_axis), position = position_nudge(x = -1.45),
            side = "l", fill = colors[2], alpha = .6) +
          geom_half_violin(
            data = data %>% filter(x_axis=="3"),aes(x = x_axis, y = y_axis), position = position_nudge(x = 1.45),
            side = "r", fill = colors[1], alpha = .6) +
          geom_half_violin(
            data = data %>% filter(x_axis=="4"),aes(x = x_axis, y = y_axis), position = position_nudge(x = .45),
            side = "r", fill = colors[2], alpha = .6)
        
      } else {
        raincloud <- raincloud +
          
          geom_half_violin(data = data %>% filter(x_axis == "1"), aes(x = x_axis, y = y_axis), position = position_nudge(x = -.24), side = "l", fill = colors[1], alpha = .6) +
          geom_half_violin(data = data %>% filter(x_axis == "2"), aes(x = x_axis, y = y_axis), position = position_nudge(x = -1.24), side = "l", fill = colors[2], alpha = .6) +
          geom_half_violin(data = data %>% filter(x_axis == "3"), aes(x = x_axis, y = y_axis), position = position_nudge(x = 1.24), side = "r", fill = colors[1], alpha = .6) +
          geom_half_violin(data = data %>% filter(x_axis == "4"), aes(x = x_axis, y = y_axis), position = position_nudge(x = .24), side = "r", fill = colors[2], alpha = .6)
        
      }
      
      
      # Additional settings
      raincloud <- raincloud + scale_color_manual(values = colors, labels = within_levels, name = within_label) +
        guides(color = guide_legend(override.aes = list(shape = 15, size = 4)))
      
      if (between_annotations == "top"){
        raincloud <- raincloud +
          scale_x_continuous(breaks = c(1, 2, 3, 4), labels = rep(within_levels, length(unique(data$between_factor))), limits = c(0, 5)) +
          annotate("text", x = 1.5, y = max(data$y_axis) * 1.05, label = between_levels[1], hjust = 0.5) +
          annotate("text", x = 3.5, y = max(data$y_axis) * 1.05, label = between_levels[2], hjust = 0.5) +
          theme_classic() +
          theme(legend.position = "none",
                axis.title.x = element_text(vjust = -3),
                axis.title.y = element_text(vjust = 3),
                plot.margin = margin(b = 15, l = 15))
        
      } else if (between_annotations == "bottom"){
        raincloud <- raincloud +
          scale_x_continuous(breaks = c(1.5, 3.5), labels = between_levels, limits = c(0, 5)) +
          theme_classic() +
          # scale_color_manual(values = colors,
          #                    labels = within_levels) +
          # guides(color = guide_legend(override.aes = list(linetype = 0, shape = 15))) +
          
          theme(legend.position = "top",
                axis.title.x = element_text(vjust = -3),
                axis.title.y = element_text(vjust = 3),
                plot.margin = margin(b = 15, l = 15))
      }
      
      return(raincloud)
      
      
    } else if (length(unique(data$between_factor)) == 3) {
      summary_df_l <- summary_df %>% filter(group == "1" | group == "2")
      summary_df_h <- summary_df %>% filter(group == "3" | group == "4")
      summary_df_3 <- summary_df %>% filter(group == "5" | group == "6")
      
      dodge1 = 0
      dodge2 = -0.25
      dodge = 0.25
      
      x_tick_means_1 <- c(.87 + dodge1, 2.15 + dodge1)
      x_tick_means_2 <- c(2.87 + dodge2, 4.15 + dodge2)
      x_tick_means_3 <- c(4.87 + dodge, 6.15 + dodge)
      
      raincloud <- ggplot(data = data, aes(y = y_axis)) +
        geom_point(shape = 16, data = data %>% filter(x_axis %in% c("1","2")), aes(x = jit, color = as.factor(x_axis)), size = p_size, alpha = .6) +
        geom_point(shape = 16, data = data %>% filter(x_axis == "3"), aes(x = jit+dodge2), color = colors[1], size = p_size, alpha = .6) +
        geom_point(shape = 16, data = data %>% filter(x_axis == "4"), aes(x = jit+dodge2), color = colors[2], size = p_size, alpha = .6) +
        geom_point(shape = 16, data = data %>% filter(x_axis == "5"), aes(x = jit+dodge), color = colors[1], size = p_size, alpha = .6) +
        geom_point(shape = 16, data = data %>% filter(x_axis == "6"), aes(x = jit+dodge), color = colors[2], size = p_size, alpha = .6) +
        
        geom_line(data = data %>% filter(x_axis %in% c("1", "2")), aes(x = jit+dodge1, group = id), color = 'lightgray', alpha = .3) +
        geom_line(data = data %>% filter(x_axis %in% c("3", "4")), aes(x = jit+dodge2, group = id), color = 'lightgray', alpha = .3) +
        geom_line(data = data %>% filter(x_axis %in% c("5", "6")), aes(x = jit + dodge, group = id), color = 'lightgray', alpha = .3) +
        
        geom_half_violin(data = data %>% filter(x_axis == "1"), aes(x = x_axis+dodge1, y = y_axis), position = position_nudge(x = -.23), side = "l", fill = colors[1], alpha = .6) +
        geom_half_violin(data = data %>% filter(x_axis == "2"), aes(x = x_axis+dodge1, y = y_axis), position = position_nudge(x = -1.23), side = "l", fill = colors[2], alpha = .6) +
        geom_half_violin(data = data %>% filter(x_axis == "3"), aes(x = x_axis+dodge2, y = y_axis), position = position_nudge(x = 1.23), side = "r", fill = colors[1], alpha = .6) +
        geom_half_violin(data = data %>% filter(x_axis == "4"), aes(x = x_axis+dodge2, y = y_axis), position = position_nudge(x = .23), side = "r", fill = colors[2], alpha = .6) +
        geom_half_violin(data = data %>% filter(x_axis == "5"), aes(x = x_axis + dodge, y = y_axis), position = position_nudge(x = -.23), side = "l", fill = colors[1], alpha = .6) +
        geom_half_violin(data = data %>% filter(x_axis == "6"), aes(x = x_axis + dodge, y = y_axis), position = position_nudge(x = -1.23), side = "l", fill = colors[2], alpha = .6) +
        
        geom_line(data = summary_df_l, aes(x = x_tick_means_1, y = score_mean), color = 'gray', linewidth = .8) +
        geom_line(data = summary_df_h, aes(x = x_tick_means_2, y = score_mean), color = 'gray', linewidth = .8) +
        geom_line(data = summary_df_3, aes(x = x_tick_means_3, y = score_mean), color = 'gray', linewidth = .8) +
        
        geom_point(shape = 16, data = data %>% filter(x_axis == "1"), aes(x = x_axis+dodge1, y = summary_df$score_mean[1]), position = position_nudge(x = -.15), color = colors[1], alpha = .6, size = meanp_size) +
        geom_errorbar(data = data %>% filter(x_axis == "1"), aes(x = x_axis+dodge1, y = summary_df$score_mean[1], ymin = summary_df$score_mean[1] - summary_df$ci[1], ymax = summary_df$score_mean[1] + summary_df$ci[1]), position = position_nudge(-.15), color = colors[1], width = 0.05, linewidth = 0.4, alpha = .6) +
        geom_point(shape = 16, data = data %>% filter(x_axis == "2"), aes(x = x_axis+dodge1, y = summary_df$score_mean[2]), position = position_nudge(x = .15), color = colors[2], alpha = .6, size = meanp_size) +
        geom_errorbar(data = data %>% filter(x_axis == "2"), aes(x = x_axis+dodge1, y = summary_df$score_mean[2], ymin = summary_df$score_mean[2] - summary_df$ci[2], ymax = summary_df$score_mean[2] + summary_df$ci[2]), position = position_nudge(x = .15), color = colors[2], width = 0.05, linewidth = 0.4, alpha = .6) +
        geom_point(shape = 16, data = data %>% filter(x_axis == "3"), aes(x = x_axis+dodge2, y = summary_df$score_mean[3]), position = position_nudge(x = -.15), color = colors[1], alpha = .6, size = meanp_size) +
        geom_errorbar(data = data %>% filter(x_axis == "3"), aes(x = x_axis+dodge2, y = summary_df$score_mean[3], ymin = summary_df$score_mean[3] - summary_df$ci[3], ymax = summary_df$score_mean[3] + summary_df$ci[3]), position = position_nudge(-.15), color = colors[1], width = 0.05, linewidth = 0.4, alpha = .6) +
        geom_point(shape = 16, data = data %>% filter(x_axis == "4"), aes(x = x_axis+dodge2, y = summary_df$score_mean[4]), position = position_nudge(x = .15), color = colors[2], alpha = .6, size = meanp_size) +
        geom_errorbar(data = data %>% filter(x_axis == "4"), aes(x = x_axis+dodge2, y = summary_df$score_mean[4], ymin = summary_df$score_mean[4] - summary_df$ci[4], ymax = summary_df$score_mean[4] + summary_df$ci[4]), position = position_nudge(.15), color = colors[2], width = 0.05, linewidth = 0.4, alpha = .6) +
        geom_point(shape = 16, data = data %>% filter(x_axis == "5"), aes(x = x_axis + dodge, y = summary_df$score_mean[5]), position = position_nudge(x = -.15), color = colors[1], alpha = .6, size = meanp_size) +
        geom_errorbar(data = data %>% filter(x_axis == "5"), aes(x = x_axis + dodge, y = summary_df$score_mean[5], ymin = summary_df$score_mean[5] - summary_df$ci[5], ymax = summary_df$score_mean[5] + summary_df$ci[5]), position = position_nudge(-.15), color = colors[1], width = 0.05, linewidth = 0.4, alpha = .6) +
        geom_point(shape = 16, data = data %>% filter(x_axis == "6"), aes(x = x_axis + dodge, y = summary_df$score_mean[6]), position = position_nudge(x = .15), color = colors[2], alpha = .6, size = meanp_size) +
        geom_errorbar(data = data %>% filter(x_axis == "6"), aes(x = x_axis + dodge, y = summary_df$score_mean[6], ymin = summary_df$score_mean[6] - summary_df$ci[6], ymax = summary_df$score_mean[6] + summary_df$ci[6]), position = position_nudge(.15), color = colors[2], width = 0.05, linewidth = 0.4, alpha = .6) +
        
        # Additional settings
        scale_color_manual(values = colors, labels = within_levels, name = within_label) +
        guides(color = guide_legend(override.aes = list(shape = 15, size = 4)))
      
      if (between_annotations == "top"){
        raincloud <- raincloud +
          scale_x_continuous(breaks = c(1+dodge1, 2+dodge1, 3+dodge2, 4+dodge2, 5 + dodge, 6 + dodge), labels = rep(within_levels, length(unique(data$between_factor))), limits = c(0, 7+dodge)) +
          annotate("text", x = 1.5+dodge1, y = max(data$y_axis) * 1.05, label = between_levels[1], hjust = 0.5) +
          annotate("text", x = 3.5+dodge2, y = max(data$y_axis) * 1.05, label = between_levels[2], hjust = 0.5) +
          annotate("text", x = 5.5+dodge, y = max(data$y_axis) * 1.05, label = between_levels[3], hjust = 0.5) +
          theme_classic() +
          theme(legend.position = "none",
                axis.title.x = element_text(vjust = -3),
                axis.title.y = element_text(vjust = 3),
                plot.margin = margin(b = 15, l = 15))
        
      } else if (between_annotations == "bottom"){
        raincloud <- raincloud +
          scale_x_continuous(breaks = c(1.5+dodge1, 3.5+dodge2, 5.5+dodge), labels = between_levels, limits = c(0, 7+dodge)) +
          theme_classic() +
          theme(legend.position = "top",
                axis.title.x = element_text(vjust = -3),
                axis.title.y = element_text(vjust = 3),
                plot.margin = margin(b = 15, l = 15))
      }
      
      return(raincloud)
      
      
    } else if (length(unique(data$between_factor)) == 4) {
      summary_df_l <- summary_df %>% filter(group == "1" | group == "2")
      summary_df_h <- summary_df %>% filter(group == "3" | group == "4")
      summary_df_3 <- summary_df %>% filter(group == "5" | group == "6")
      summary_df_4 <- summary_df %>% filter(group == "7" | group == "8")
      
      dodge1 = 0
      dodge2 = -0.25
      dodge = 0.25
      dodge4 = 0
      
      x_tick_means_1 <- c(.87 + dodge1, 2.15 + dodge1)
      x_tick_means_2 <- c(2.87 + dodge2, 4.15 + dodge2)
      x_tick_means_3 <- c(4.87 + dodge, 6.15 + dodge)
      x_tick_means_4 <- c(6.87 + dodge4, 8.15 + dodge4)
      
      
      raincloud <- ggplot(data = data, aes(y = y_axis)) +
        geom_point(shape = 16, data = data %>% filter(x_axis %in% c("1","2")), aes(x = jit, color = as.factor(x_axis)), size = p_size, alpha = .6) +
        geom_point(shape = 16, data = data %>% filter(x_axis == "3"), aes(x = jit+dodge2), color = colors[1], size = p_size, alpha = .6) +
        geom_point(shape = 16, data = data %>% filter(x_axis == "4"), aes(x = jit+dodge2), color = colors[2], size = p_size, alpha = .6) +
        geom_point(shape = 16, data = data %>% filter(x_axis == "5"), aes(x = jit+dodge), color = colors[1], size = p_size, alpha = .6) +
        geom_point(shape = 16, data = data %>% filter(x_axis == "6"), aes(x = jit+dodge), color = colors[2], size = p_size, alpha = .6) +
        geom_point(shape = 16, data = data %>% filter(x_axis == "7"), aes(x = jit+dodge4), color = colors[1], size = p_size, alpha = .6) +
        geom_point(shape = 16, data = data %>% filter(x_axis == "8"), aes(x = jit+dodge4), color = colors[2], size = p_size, alpha = .6) +
        
        geom_line(data = data %>% filter(x_axis %in% c("1", "2")), aes(x = jit+dodge1, group = id), color = 'lightgray', alpha = .3) +
        geom_line(data = data %>% filter(x_axis %in% c("3", "4")), aes(x = jit+dodge2, group = id), color = 'lightgray', alpha = .3) +
        geom_line(data = data %>% filter(x_axis %in% c("5", "6")), aes(x = jit + dodge, group = id), color = 'lightgray', alpha = .3) +
        geom_line(data = data %>% filter(x_axis %in% c("7", "8")), aes(x = jit + dodge4, group = id), color = 'lightgray', alpha = .3) +
        
        geom_half_violin(data = data %>% filter(x_axis == "1"), aes(x = x_axis+dodge1, y = y_axis), position = position_nudge(x = -.24), side = "l", fill = colors[1], alpha = .6) +
        geom_half_violin(data = data %>% filter(x_axis == "2"), aes(x = x_axis+dodge1, y = y_axis), position = position_nudge(x = -1.24), side = "l", fill = colors[2], alpha = .6) +
        geom_half_violin(data = data %>% filter(x_axis == "3"), aes(x = x_axis+dodge2, y = y_axis), position = position_nudge(x = 1.24), side = "r", fill = colors[1], alpha = .6) +
        geom_half_violin(data = data %>% filter(x_axis == "4"), aes(x = x_axis+dodge2, y = y_axis), position = position_nudge(x = .24), side = "r", fill = colors[2], alpha = .6) +
        geom_half_violin(data = data %>% filter(x_axis == "5"), aes(x = x_axis + dodge, y = y_axis), position = position_nudge(x = -.24), side = "l", fill = colors[1], alpha = .6) +
        geom_half_violin(data = data %>% filter(x_axis == "6"), aes(x = x_axis + dodge, y = y_axis), position = position_nudge(x = -1.24), side = "l", fill = colors[2], alpha = .6) +
        geom_half_violin(data = data %>% filter(x_axis == "7"), aes(x = x_axis + dodge4, y = y_axis), position = position_nudge(x = 1.24), side = "r", fill = colors[1], alpha = .6) +
        geom_half_violin(data = data %>% filter(x_axis == "8"), aes(x = x_axis + dodge4, y = y_axis), position = position_nudge(x = .24), side = "r", fill = colors[2], alpha = .6) +
        
        geom_line(data = summary_df_l, aes(x = x_tick_means_1, y = score_mean), color = 'gray', linewidth = .8) +
        geom_line(data = summary_df_h, aes(x = x_tick_means_2, y = score_mean), color = 'gray', linewidth = .8) +
        geom_line(data = summary_df_3, aes(x = x_tick_means_3, y = score_mean), color = 'gray', linewidth = .8) +
        geom_line(data = summary_df_4, aes(x = x_tick_means_4, y = score_mean), color = 'gray', linewidth = .8) +
        
        geom_point(shape = 16, data = data %>% filter(x_axis == "1"), aes(x = x_axis+dodge1, y = summary_df$score_mean[1]), position = position_nudge(x = -.15), color = colors[1], alpha = .6, size = meanp_size) +
        geom_errorbar(data = data %>% filter(x_axis == "1"), aes(x = x_axis+dodge1, y = summary_df$score_mean[1], ymin = summary_df$score_mean[1] - summary_df$ci[1], ymax = summary_df$score_mean[1] + summary_df$ci[1]), position = position_nudge(-.15), color = colors[1], width = 0.05, linewidth = 0.4, alpha = .6) +
        geom_point(shape = 16, data = data %>% filter(x_axis == "2"), aes(x = x_axis+dodge1, y = summary_df$score_mean[2]), position = position_nudge(x = .15), color = colors[2], alpha = .6, size = meanp_size) +
        geom_errorbar(data = data %>% filter(x_axis == "2"), aes(x = x_axis+dodge1, y = summary_df$score_mean[2], ymin = summary_df$score_mean[2] - summary_df$ci[2], ymax = summary_df$score_mean[2] + summary_df$ci[2]), position = position_nudge(x = .15), color = colors[2], width = 0.05, linewidth = 0.4, alpha = .6) +
        geom_point(shape = 16, data = data %>% filter(x_axis == "3"), aes(x = x_axis+dodge2, y = summary_df$score_mean[3]), position = position_nudge(x = -.15), color = colors[1], alpha = .6, size = meanp_size) +
        geom_errorbar(data = data %>% filter(x_axis == "3"), aes(x = x_axis+dodge2, y = summary_df$score_mean[3], ymin = summary_df$score_mean[3] - summary_df$ci[3], ymax = summary_df$score_mean[3] + summary_df$ci[3]), position = position_nudge(-.15), color = colors[1], width = 0.05, linewidth = 0.4, alpha = .6) +
        geom_point(shape = 16, data = data %>% filter(x_axis == "4"), aes(x = x_axis+dodge2, y = summary_df$score_mean[4]), position = position_nudge(x = .15), color = colors[2], alpha = .6, size = meanp_size) +
        geom_errorbar(data = data %>% filter(x_axis == "4"), aes(x = x_axis+dodge2, y = summary_df$score_mean[4], ymin = summary_df$score_mean[4] - summary_df$ci[4], ymax = summary_df$score_mean[4] + summary_df$ci[4]), position = position_nudge(.15), color = colors[2], width = 0.05, linewidth = 0.4, alpha = .6) +
        geom_point(shape = 16, data = data %>% filter(x_axis == "5"), aes(x = x_axis + dodge, y = summary_df$score_mean[5]), position = position_nudge(x = -.15), color = colors[1], alpha = .6, size = meanp_size) +
        geom_errorbar(data = data %>% filter(x_axis == "5"), aes(x = x_axis + dodge, y = summary_df$score_mean[5], ymin = summary_df$score_mean[5] - summary_df$ci[5], ymax = summary_df$score_mean[5] + summary_df$ci[5]), position = position_nudge(-.15), color = colors[1], width = 0.05, linewidth = 0.4, alpha = .6) +
        geom_point(shape = 16, data = data %>% filter(x_axis == "6"), aes(x = x_axis + dodge, y = summary_df$score_mean[6]), position = position_nudge(x = .15), color = colors[2], alpha = .6, size = meanp_size) +
        geom_errorbar(data = data %>% filter(x_axis == "6"), aes(x = x_axis + dodge, y = summary_df$score_mean[6], ymin = summary_df$score_mean[6] - summary_df$ci[6], ymax = summary_df$score_mean[6] + summary_df$ci[6]), position = position_nudge(.15), color = colors[2], width = 0.05, linewidth = 0.4, alpha = .6) +
        geom_point(shape = 16, data = data %>% filter(x_axis == "7"), aes(x = x_axis + dodge4, y = summary_df$score_mean[7]), position = position_nudge(x = -.15), color = colors[1], alpha = .6, size = meanp_size) +
        geom_errorbar(data = data %>% filter(x_axis == "7"), aes(x = x_axis + dodge4, y = summary_df$score_mean[7], ymin = summary_df$score_mean[7] - summary_df$ci[7], ymax = summary_df$score_mean[7] + summary_df$ci[7]), position = position_nudge(-.15), color = colors[1], width = 0.05, linewidth = 0.4, alpha = .6) +
        geom_point(shape = 16, data = data %>% filter(x_axis == "8"), aes(x = x_axis + dodge4, y = summary_df$score_mean[8]), position = position_nudge(x = .15), color = colors[2], alpha = .6, size = meanp_size) +
        geom_errorbar(data = data %>% filter(x_axis == "8"), aes(x = x_axis + dodge4, y = summary_df$score_mean[8], ymin = summary_df$score_mean[8] - summary_df$ci[8], ymax = summary_df$score_mean[8] + summary_df$ci[8]), position = position_nudge(.15), color = colors[2], width = 0.05, linewidth = 0.4, alpha = .6) +
        
        
        # Additional settings
        scale_color_manual(values = colors, labels = within_levels, name = within_label) +
        guides(color = guide_legend(override.aes = list(shape = 15, size = 4)))
      
      if (between_annotations == "top"){
        raincloud <- raincloud +
          scale_x_continuous(breaks = c(1+dodge1, 2+dodge1, 3+dodge2, 4+dodge2, 5 + dodge, 6 + dodge, 7 + dodge4, 8 + dodge4), labels = rep(within_levels, length(unique(data$between_factor))), limits = c(0, 9+ dodge4)) +
          annotate("text", x = 1.5+dodge1, y = max(data$y_axis) * 1.05, label = between_levels[1], hjust = 0.5) +
          annotate("text", x = 3.5+dodge2, y = max(data$y_axis) * 1.05, label = between_levels[2], hjust = 0.5) +
          annotate("text", x = 5.5+dodge, y = max(data$y_axis) * 1.05, label = between_levels[3], hjust = 0.5) +
          annotate("text", x = 7.5+dodge4, y = max(data$y_axis) * 1.05, label = between_levels[4], hjust = 0.5) +
          theme_classic() +
          theme(legend.position = "none",
                axis.title.x = element_text(vjust = -3),
                axis.title.y = element_text(vjust = 3),
                plot.margin = margin(b = 15, l = 15))
        
      } else if (between_annotations == "bottom"){
        raincloud <- raincloud +
          scale_x_continuous(breaks = c(1.5+dodge1, 3.5+dodge2, 5.5+dodge, 7.5+dodge4), labels = between_levels, limits = c(0, 9+dodge)) +
          theme_classic() +
          theme(legend.position = "top",
                axis.title.x = element_text(vjust = -3),
                axis.title.y = element_text(vjust = 3),
                plot.margin = margin(b = 15, l = 15))
      }
      
      return(raincloud)
      
    }
    
    
  }
}



