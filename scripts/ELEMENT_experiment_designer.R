###############################################
########## Experimental Designer ##############
###############################################

#### Script that has the function that analyzes available data + user inputs to create an experimental design

generate_exper_design <- function(survival_data = survival_data,
                                  site_type = "Casa del ojo",
                                  species_type = c("Parkinsonia aculeata"),
                                  species_num = "50",
                                  restor_prop = 0.5,
                                  return_prev_plantings = F,
                                  return_message = F){
  
  messages <- NULL
  
  # Parse the species_num input:
  # remove white space from spp_number string:
  species_num <- str_replace_all(species_num, fixed(" "), "")
  # then separate by comma:
  species_num <- as.numeric(str_split(species_num, ",")[[1]])
  
  # Then parse/summarize the data to get just lifespan of each individual plant
  survival_data_parsed <- dplyr::select(survival_data, date = monitoring_date, plant_ID, treatment, species, alive, site) %>% 
    group_by(plant_ID) %>%
    mutate(life_span = as.numeric(max(date) - min(date)),
           died = as.numeric(any(alive == "no"))) %>% 
    summarize(planting_date = min(date), treatment = unique(treatment), 
              species = unique(species), life_span = unique(life_span), 
              died = unique(died), planting_date = unique(planting_date),
              last_date = max(date), site = unique(site)) %>% 
    mutate(treatment = as.factor(treatment)) %>% 
    dplyr::select(species, treatment, life_span, died, planting_date, last_date, site) %>% 
    dplyr::ungroup()
  
  # Now filter out the species and sites that are selected in the experiment designer:
  surv_data_for_design <- filter(survival_data_parsed, species %in% species_type, site == site_type)
  
  # Extract a table of all last plantings of those species at that site:
  # but if there were no last plantings:
  if(nrow(surv_data_for_design) == 0){
    # previous_plantings <- tibble(species = species_type, any_treatments = 0)
    previous_plantings <- table(survival_data$species, survival_data$treatment)
    previous_plantings <- previous_plantings[row.names(previous_plantings) %in% species_type,]
    previous_plantings <- previous_plantings*0
    previous_plantings <- as.data.frame(tibble(!!!previous_plantings))
    #if(nrow(previous_plantings)<=1) previous_plantings <- as.data.frame(tibble(!!!previous_plantings))
    row.names(previous_plantings) <- species_type
    previous_plantings <- as.data.frame.matrix(previous_plantings)
  } else {
    previous_plantings <- table(surv_data_for_design$species, surv_data_for_design$treatment)
    previous_plantings <- as.data.frame.matrix(previous_plantings)
  }
  
  if(return_prev_plantings) return(previous_plantings)
  
  # all previous potential treatments:
  all_trts <- names(previous_plantings)
  
  # Hover how many months are we evaluating survival? 
  # Eventually will have this be input by the user:
  months = 12
  # First, set an interval of time in months:
  period = (months*30.4375)
  
  # Then prepare the data for analysis:
  # Then remove all plants planted within the last period (that haven't had at least that amount of time to grow):
  # For everything else, we can ask: did it survive after growing for that period of time?
  # but first check if there are actually any previous data available:
  # if not, then return message saying it's a new species for the site
  
  if(nrow(surv_data_for_design) == 0){
    #all_spp_fin_design <- previous_plantings
    
    
    
    
    # Loop through each species:
    all_spp_fin_design <- NULL
    species_names <- NULL
    for(s in 1:length(species_type)){
      current_spp <- species_type[s]
      prev_numbers <- previous_plantings[s,]
      
      # All trts used in the past for this species at this site (remove any where prev_numbers == 0)
      prev_trts <- names(prev_numbers)[which(prev_numbers > 0)]
      
      # Replace those zeros with NAs
      prev_numbers[which(prev_numbers==0)] <- NA
      
      # Create an empty design:
      # But if all NAs, then replace NAs with zeros
      if(sum(!is.na(prev_numbers))==0){
        # How many treatments were previously use in other sites/species overall?
        trt_num <- length(all_trts)
        prev_numbers[,is.na(prev_numbers)] <- 0
        messages <- c(messages, paste("There were no previous plantings at this site for",current_spp,
                                      "\nso a balanced design was created using all potential treatments."))

      } else {
        # How many treatments were previously used?
        trt_num <- length(prev_trts)
      }
      

      empty_design <- prev_numbers*0
      
      # Number of plants available to plant for this species:
      to_plant <- species_num[s]
      
      # Create a balanced design using total available plantings
      balanced_design <- empty_design + ceiling(to_plant/trt_num)
      
      # Because the previous step rounded up to ensure all plants are used, it is
      # possible that more plants were added to the design than there are available.
      # This can happen if the number of plants is not neatly divisible by the number of treatments
      # So now loop through the data and randomly remove individuals until reaching the original sample
      while(sum(balanced_design, na.rm=T) > to_plant){
        random_pick <- sample(which(!is.na(balanced_design)), 1)
        balanced_design[random_pick] <- balanced_design[random_pick] - 1
      }
      
      final_design <- balanced_design
      # Then append the results for the current species into a table for all species
      all_spp_fin_design <- rbind(all_spp_fin_design, final_design)
      species_names <- c(species_names, current_spp)
    }
    
    # Finally, rename the rows in the final dataframe to the species names:
    row.names(all_spp_fin_design) <- species_names
    
  } else {
    max_last_date <- max(surv_data_for_design$last_date)
    surv_data_for_analysis <- filter(surv_data_for_design, planting_date <= max_last_date - period) %>% 
      mutate(survived_period = as.numeric(life_span > period))
    
    # sub_survival_data_filter <- filter(sub_surv_data, species %in% species_type, site == site_type)
    
    
    # Function to extract inverse proportions:
    inv_prop <- function(x) 1 - x/sum(x) 
    prop <- function(x) x/sum(x) 
    
    # Loop through each species:
    all_spp_fin_design <- NULL
    species_names <- NULL
    for(s in 1:length(species_type)){
      current_spp <- species_type[s]
      prev_numbers <- previous_plantings[s,]
      
      # All trts used in the past for this species at this site (remove any where prev_numbers == 0)
      prev_trts <- names(prev_numbers)[which(prev_numbers > 0)]
      
      # Replace those zeros with NAs
      prev_numbers[which(prev_numbers==0)] <- NA
      
      # How many treatments were previously use?
      trt_num <- length(prev_trts)
      
      # Create an empty design:
      empty_design <- prev_numbers*0
      
      # Number of plants available to plant for this species:
      to_plant <- species_num[s]
      
      # Create a balanced design using total available plantings 
      balanced_design <- empty_design + ceiling(to_plant/trt_num)
      
      # Because the previous step rounded up to ensure all plants are used, it is
      # possible that more plants were added to the design than there are available.
      # This can happen if the number of plants is not neatly divisible by the number of treatments
      # So now loop through the data and randomly remove individuals until reaching the original sample
      while(sum(balanced_design, na.rm=T) > to_plant){
        random_pick <- sample(which(!is.na(balanced_design)), 1)
        balanced_design[random_pick] <- balanced_design[random_pick] - 1
      }
      
      # Run a GLM model to test which treatment is best for plant survival in rank order:
      # First subset the analysis data just to the species we are currently on in the loop:
      data_sub <- filter(surv_data_for_analysis, species == current_spp)
      
      glm_model <- glm(data=data_sub, survived_period ~ treatment, family="binomial")
      mod_summary <- summary(glm_model)
      p_vals <- mod_summary$coefficients[,4]
      
      # Then use the model to predict mean survival for those treatments with significant p-values:
      mean_survival <- predict(glm_model, data.frame(treatment = prev_trts), type = "response")
      # Make all survival values zero if p-value was greater than 0.05 since they are not significantly different from zero:
      mean_survival_mod <- mean_survival * as.numeric(p_vals < 0.05)
      
      # Next, turn the resulting survival probabilities into proportions:
      if(sum(mean_survival_mod)==0) { # If all are zero (meaning there was no statistical significance),
        # then make them all into 1 (a balanced design):  
        mean_surv_prop <- mean_survival_mod + 1
      } else { # Otherwise, calculate the proportional importance of each treatment:
        mean_surv_prop <- mean_survival_mod/sum(mean_survival_mod)
      }
      
      # Also calculate an inverse importance of each treatment:
      inverse_prob <- (1-mean_surv_prop)/sum(1-mean_surv_prop)
      
      
      ## Slider bar effect:
      # If there is one best treatment, the slider bar determines the percentage of individuals taken from
      # the other treatments and added to the best treatment. 1 means all of the other treatment replicates
      # are added to the best treatment, and 0 means none are. Since this process of shuffling replicates
      # begins with a balanced design, a 0 maintains the balanced design:
      
      # First count how many replicates are NOT in the best treatment:
      # If mean predicted survival is zero for all the treatments, then all are best, so total_not_best = 0
      if(sum(mean_survival_mod)==0){
        total_not_best_reps <- 0
      } else { # otherwise, count how many replicates are NOT the best treatment:
        total_not_best_reps <- sum(balanced_design[which(mean_surv_prop != max(mean_surv_prop))])
      }
      # Then calculate the total number of replicates to reshuffle based on the slider bar input (0-1)
      # and round the result (doesn't have to be perfectly precise):
      restor_plants <- round(total_not_best_reps*restor_prop)
      
      # In otherwords, restor_prop of 0.5 means 50% of the non-best treatment replicates get added to the best treatment,
      # not 50% of all plants.
      
      if(restor_plants==0){ # if restor_plants is zero, then revert to the balanced design
        final_design <- balanced_design
      } else { # otherwise, start with the balanced design, but then loop through the design for each replicate 
        # that needs to be reshuffled
        final_design <- balanced_design
        for(i in 1:restor_plants){
          
          # with each run make sure to modify the inverse probability vector
          # so that when a treatment has zero replicates, you have zero probability 
          # of removing additional replicates: 
          inverse_prob_mod <- inverse_prob
          inverse_prob_mod[which(final_design==0)] <- 0
          # also always make the best treatment
          # into zero so that you never remove from the best treatment
          inverse_prob_mod[which.max(mean_surv_prop)] <- 0
          
          # pick a random treatment to remove a plant from, but based on
          # how bad it is for survival (inverse of mean_surv_prop).
          # In other words, higher chance of removing plants from treatments with
          # lower survival than those with higher:
          remove_plant <- sample(1:length(inverse_prob_mod), 1, prob=inverse_prob_mod)
          final_design[remove_plant] <- final_design[remove_plant] -1
          
          # Then add that replicate to the best treatment
          final_design[which.max(mean_surv_prop)] <- final_design[which.max(mean_surv_prop)] + 1
        }
      }
      
      # Then append the results for the current species into a table for all species
      all_spp_fin_design <- rbind(all_spp_fin_design, final_design)
      species_names <- c(species_names, current_spp)
    }
    
    # Finally, rename the rows in the final dataframe to the species names:
    row.names(all_spp_fin_design) <- species_names
    
  }
    
  if(return_message) return(messages)
  return(all_spp_fin_design)
}

# generate_exper_design(survival_data = survival_data,
#                                   site_type = "Casa del ojo",
#                                   species_type = c("Acacia macracantha","Parkinsonia aculeata"),
#                                   species_num = "50,50",
#                                   restor_prop = 0.5,
#                                   return_prev_plantings = F,
#                                   return_message = T)
# 
# generate_exper_design(survival_data = survival_data,
#                       site_type = "Casa del ojo",
#                       species_type = c("Acacia macracantha"),
#                       species_num = "50",
#                       restor_prop = 0.5,
#                       return_prev_plantings = F,
#                       return_message = T)

# generate_exper_design(survival_data = survival_data,
#                       restor_prop = 0.5,
#                       species_num = "50",
#                       site_type = "Casa de piedra",
#                       species_type = "Maytenus octogona",
#                       return_prev_plantings = F)
# 
# generate_exper_design(survival_data = survival_data,
#                       site_type = "Casa de piedra",
#                       species_type = c("Lycium minimum"),
#                       species_num = "50",
#                       restor_prop = 0.5,
#                       return_prev_plantings = T)


