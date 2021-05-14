################################################
##### UPLOAD CUSTOM FUNCTIONS AND PACKAGES #####
################################################

### PACKAGES:
library(lubridate)
library(tableHTML)
library(leaflet)
library(grid)
# For some reason won't publish without these libraries:
library(janitor)
library(snakecase)
library(tidyverse)
library(shiny)
library(shinyWidgets) 
library(shinydashboard)
library(tidyverse)
library(cowplot)
library(plotly)
library(leaflet)
library(leaflet.extras)
library(boot)
library(survival)
library(survminer)
library(lubridate)
library(multcomp)
library(broom)
library(tidyverse)


### FUNCTIONS:

### function to standardize continuous variables for effect size comparison to logical variables
### and to ensure standardization within each species for global comparison
### https://www.listendata.com/2017/04/how-to-standardize-variable-in-regression.html
standard <- function(x) (x - mean(x, na.rm=T)) / (2*sd(x, na.rm=T))

### Function to simplify species names to two words:
simp_species <- function(x) gsub("^((\\w+\\W+){1}\\w+).*$","\\1",x)

### Function to calculate the age of a plant (in days) at each sample point:
plant_age <- function(dates){ # x is the vector of sample dates
  planting_date <- min(dates) # find the planting date (earliest date)
  ages <- dates-planting_date# subtract the planting date from each other date
  return(ages)
}

### Function to calculate age of death:
death_age <- function(plant_ages, states){ # x is the vector of sample dates
  dead_mark <- which(states=="dead")
  if(length(dead_mark)>0){
    if(length(plant_ages) > 1){
      age_last_alive <- as.numeric(plant_ages[min(dead_mark)-1])
      age_first_dead <- as.numeric(plant_ages[min(dead_mark)])
    } else {
      age_last_alive <- 0
      age_first_dead <- 0
    }
    # best death age estimate is the midpoint between date last seen alive and date recorded as dead:
    death_age <- round(mean(c(age_last_alive, age_first_dead)))
  } else {
    death_age <- NA
  }
  return(death_age)
}

### Function to calculate the number of days since each last observation:
days_btw_observ <- function(plant_ages){
  days_btw <- NULL
  for(i in order(plant_ages)){ # loop through each plant age in order
    if(i == 1) days_btw[i] <-  0 # for the first one (age == 0), days between observations is 0
    else days_btw[i] <- plant_ages[i] - plant_ages[i-1]
  }
  return(days_btw)
}

### Function to extract cumulative growth (essentially the same as the plant_age function):
cum_growth <- function(height,age){ # x is the vector of sample dates
  initial_ht <- min(height) # find the initial height
  cum_growth <- height-initial_ht # subtract the initial height from each other height
  return(cum_growth)
}

### Function to label as monitored
monitor_lab <- function(death_age, date){
  max_date <- max(date)
  today <- Sys.Date()
  
  # T or F: did more than 19 months go by since last monitoring?
  year_old <- any(max_date < today-days(round(30.5*19, digits = 0)))
  
  # T or F: is the plant still alive?
  alive <- any(is.na(death_age))
  
  # if both alive and a year old
  if(year_old & alive){
    return("not_monitoring")
  } else {
    return("monitoring")
  }
}

### Function to label each plant with the planting expedition:
label_expeditions <- function(date, plant_ID, plant_age, site_name){
  site <- tibble(date, plant_ID, plant_age)
  plants <- filter(site, plant_age==0)
  expedition_list <- NULL
  site_simp <- arrange(plants, date) %>%
    select(date, plant_ID)
  expedition <- NULL
  site_counter <- 1
  for(i in 1:nrow(site_simp)){
    if(i != 1){ # as long as it is not the first row in the data...
      if(site_simp$date[i] - site_simp$date[i-1] > 5) { # if the time difference between monitorings is greater than 5 days, then its a new expedition
        site_counter <- site_counter + 1
      }
    }
    expedition[i] <- paste(gsub(" ", "_", unique(site_name)), site_counter, sep="_")
  }
  site_simp$expedition <- expedition
  site_dat <- left_join(site, site_simp[,2:3], by="plant_ID")
  return(site_dat$expedition)
}

### Custom ggplot theme:
simple_theme <- list(
  theme_classic() +
    theme(plot.margin = unit(c(.7,.7,.7,.7), "cm"),
          strip.background = element_rect(color = NA),
          axis.text.x = element_text(size = 11, angle = 45, hjust = 1),
          axis.text.y = element_text(size = 11),
          axis.title.x = element_text(size = 16, margin=ggplot2::margin(t=0.5, unit="cm")),
          axis.title.y = element_text(size = 16, margin=ggplot2::margin(r=0.5, unit="cm")),
          legend.title = element_text(size = 15),
          legend.text = element_text(size = 13),
          plot.title = element_text(size = 16),
          axis.line=element_line(),
          panel.spacing = unit(1, "lines"),
          strip.text.x = element_text(size = 18, colour = "black", angle = 0)),

  #scale_color_manual(values= c("darkgrey","black")),
  scale_shape_manual(values=c(17,16))
)

### Function to get islands with their sites as a list
sites_list_fun <- function(data){
  
  # df with islands and site names
  islands_sites <- data %>% 
    select(island, site) %>%
    filter(!(site %in% not_monitoring_fun(data))) %>% 
    unique()
  
  # create list with islands and site names
  sites_list <- split(islands_sites$site, islands_sites$island)
  
  return(sites_list)
}

### Function to get vector of sites not being monitored anymore (not monitored label + manually removed)
not_monitoring_fun <- function(data){
  
  # in case "monitoring_date" column is missing, add it in
  if(!("monitoring_date" %in% names(data))) rename(data, monitoring_date = date)
  
  # With monitoring label == not monitoring
  not_monitoring_label <- data %>%
    group_by(plant_ID) %>%
    filter(monitoring_date == max(monitoring_date)) %>%
    group_by(site) %>%
    summarise(total_monitored = sum(monitored == 'monitoring' &
                                      state != "dead")) %>%
    filter(total_monitored == 0)
  
  # Manually removed
  not_monitoring_manual <- c(
    'Finca Nixon López (Occidente)',
    'Finca Lucia llerena',
    'Finca  Wilson Cabrera',
    'Hotel Pikaia',
    'Garrapatero',
    'Capitania de Puerto Ayora',
    'Perla Solar',
    'Sr. Eddy Rosero'
  )
  
  # not monitoring label + manually removed
  sites_not_monitoring <- c(as.character(not_monitoring_label[[1]]),
                            not_monitoring_manual)
  # return vector
  return(sites_not_monitoring)
}

### Function to label monitoring expeditions
label_monitoring_exp <- function(plant_ID, monitoring_date) {
  
  # Create tibble with all obs for a specific site
  site <- tibble(plant_ID, monitoring_date)
  
  # Create df with unique dates (expeditions will be added to this df)
  dates_df <- site %>%
    dplyr::select(monitoring_date) %>%
    unique() %>% 
    arrange(monitoring_date) # dates need to be ordered for the for loop to work correctly
  
  # Add column for expeditions that will be populated by the for loop
  dates_df$monitoring_exp <- NA
  
  # Inialize variables
  counter <- 1
  grouped_IDs <- c(0, 0)
  
  # For loop
  for (i in 1:length(dates_df$monitoring_date)) {
    # treatment IDs for a specific date
    obs_date <-  site %>%
      filter(monitoring_date == dates_df$monitoring_date[i]) %>%
      dplyr::select(plant_ID)
    
    # create vector w/ treatment IDs
    obs_date_v <- c(obs_date$plant_ID)
    
    # if statement to compare treatment IDs
    if (any(obs_date_v %in% grouped_IDs)) {
      grouped_IDs <- obs_date_v
      counter <- counter + 1
    } else {
      grouped_IDs <- c(grouped_IDs, obs_date_v)
    }
    # Add expedition number to dates df
    dates_df$monitoring_exp[i] <- counter
    
  }
  ##### end for loop ####
  
  # Add monitoring expeditions to site tibble
  site_w_exp <- full_join(site, dates_df, by = "monitoring_date")
  
  # Return monitoring expeditions
  return(site_w_exp$monitoring_exp)
}





