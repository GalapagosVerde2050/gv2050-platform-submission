#################################################
##### FUNCTIONS FOR THE MONITORING CALENDAR #####
#################################################

### Function to generate the monitoring calendar:
monitor_schedule <- function(cleaned_data = sample_data) {
  
  # Df with last monitoring date for each site and total number of plants alive
  df_monitoring <- cleaned_data %>% 
    group_by(plant_ID) %>%
    filter(monitoring_date == max(monitoring_date)) %>%
    ungroup() %>%
    group_by(site) %>%
    mutate(last_monitoring = max(monitoring_date)) %>% # create column for lastest monitoring expedition
    summarize(
      last_monitoring = unique(last_monitoring),
      total_alive = sum(state != "dead")) %>% #only want the plants that are currently alive
    #used recode to assign names to the values of 1 or 0
    ungroup() %>%
    mutate(months_since = (Sys.Date() - last_monitoring) /
             30.5) 
  
  # Add colors to df of sites
  df_w_colors <- arrange(df_monitoring, months_since) %>%
    mutate(
      site = factor(site, levels = unique(site)),
      color = ifelse(months_since < 3, "#A5C134", "black"),
      color = ifelse(months_since >= 3 &
                       months_since < 6, "#FFC700", color),
      color = ifelse(months_since >= 6, "#BC1212", color)
    )
  
  # Bar plot 
  sites_calendar <-plot_ly(df_w_colors) %>%
    add_bars(
      x = ~ months_since,
      y = ~ site,
      marker = list(color = c(df_w_colors$color)),
      orientation = 'h'
    ) %>%
    layout(
      xaxis = list(title = "Months since last monitoring",
                   dtick = 1),
      yaxis = list(title = ""),
      showlegend = FALSE
    )
  
  return(sites_calendar)
  
}