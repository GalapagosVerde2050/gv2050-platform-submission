###############################################
### FUNCTIONS FOR RESTORATION TARGET FIGURE ###
###############################################

target_plot <- function(dat_all = sample_data, target){
  unique_all <- vector()
  cum_planted <- vector()
  unique_dead_all <- vector()
  cum_died <- vector()
  for(i in sort(unique(dat_all$monitoring_date))) {
    date_dat <- filter(dat_all, monitoring_date==i)
    unique_IDs <- unique(date_dat$plant_ID)
    unique_all <- unique(c(unique_all,unique_IDs))
    cum_planted <- c(cum_planted,length(unique_all))
    
    unique_dead_IDs <- date_dat$plant_ID[date_dat$state=="dead"]
    unique_dead_all <- unique(c(unique_dead_all,unique_dead_IDs))
    cum_died <- c(cum_died,length(unique_dead_all))
  }
  
  # Data for graph
  dates <- sort(unique(dat_all$monitoring_date))
  restor_data <- tibble(dates,cum_planted,cum_died,cum_alive=cum_planted-cum_died)
  
  restor_dat <- restor_data %>% 
    gather('cum_state', 'total_plants', 3:4)
  
  # Graph using ggplot
 target_graph <-  ggplot(restor_dat, aes(x = dates, y = total_plants, group = cum_state, fill = cum_state)) +
    geom_area(position = position_stack(reverse = TRUE)) +
    geom_hline(yintercept = target,
               color = 'red',
               size = 2) +
    theme_classic() +
    scale_y_continuous(expand = c(0,0)) +
    scale_fill_manual(values = c(GV2050_colors("GVgreen")[[1]], GV2050_colors("dead")[[1]])) +
    labs(x = "Year",
         y = "Number of Plants") +
    theme(legend.position = "none",
          axis.text = element_text(size=16),
          axis.title = element_text(size=18)) +
   annotate("text",
            x = min(dates),
            y = target + 200,
            label = paste0("Target: ", target, " plants"),
            hjust = "left",
            color = "red",
            size = 6) +
   annotate("text",
            x = min(dates),
            y = max(restor_data$cum_planted)-200,
            label = paste0("Total planted: ", max(restor_data$cum_planted), " plants"),
            hjust = "left",
            color = GV2050_colors("dead")[[1]],
            size = 6) +
   annotate("text",
            x = min(dates),
            y = max(restor_data$cum_planted) - 500,
            label = paste0("Total alive: ", restor_data$cum_alive[length(restor_data$cum_alive)], " plants"),
            hjust = "left",
            color = GV2050_colors("GVgreen")[[1]],
            size = 6)
 
 print(target_graph)
}