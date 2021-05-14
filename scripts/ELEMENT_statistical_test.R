###############################################
####### FUNCTIONS FOR STATISTICAL TESTS #######
###############################################
### Function to test for a significant difference between treatments in the dataset

# Assumptions of the test are:
# 1) The plantings are independent and based on an experimental design in which the 
#    treatments were randomized or at least spatially and temporally independent.
# 2) The goal is to evaluate survival of planted individuals and find what
#    treatment works best and by what degree

#data <- read_csv("data/hypothetical_data.csv")

# Two options might be:
# 1) a basic kaplan-meier survival curve
# 2) a simple logistic regression with a plot showing survival probability at several points in time

# Now function to plot the KM results
KM_surv_curv <- function(data = sample_data, all_spp = F){
  # first create the survival data:
  # good reference: https://www.emilyzabor.com/tutorials/survival_analysis_in_r_tutorial.html
  surv_data <- dplyr::select(data, date = monitoring_date, plant_ID, treatment, species, alive) %>% 
    group_by(plant_ID) %>% 
    mutate(life_span = as.numeric(max(date) - min(date)),
           died = as.numeric(any(alive == "no"))) %>% 
    summarize(planting_date = min(date), treatment = unique(treatment), 
              species = unique(species), life_span = unique(life_span), 
              died = unique(died), planting_date = unique(planting_date),
              last_date = max(date)) %>% 
    mutate(treatment = as.factor(treatment)) %>% 
    dplyr::select(species, treatment, life_span, died, planting_date, last_date)
  max_last_date <- max(surv_data$last_date)
  
  
  f2 <- survfit(Surv(life_span, died) ~ treatment, data = surv_data)
  leg_labs <- str_to_title(levels(surv_data$treatment))
  
  if(all_spp){
    graphic <- ggsurvplot_facet(
      data = surv_data,
      facet.by = "species",
      fit = f2, 
      xlab = "Months since planting", 
      ylab = "Survival Probability (±95% CI)",
      conf.int = T,
      legend = "right",
      legend.title = "",
      legend.labs = leg_labs,
      surv.scale = "percent",
      xscale = "d_m",
      axes.offset = F,
      conf.int.style = "ribbon",
      censor = F,
      ggtheme = theme_classic(),
      break.time.by = 365.25,
      font.y=c(15),
      font.x=c(15),
      font.tickslab=c(12,"bold"),
      font.legend=c(13, "bold"),
      font.title=c(17,"italic"),
      palette = "jco"
    )
  } else {
    graphic <- ggsurvplot(
      data = surv_data,
      fit = f2, 
      xlab = "Months since planting", 
      ylab = "Survival Probability (±95% CI)",
      conf.int = T,
      legend = "right",
      legend.title = "",
      legend.labs = leg_labs,
      surv.scale = "percent",
      xscale = "d_m",
      axes.offset = F,
      conf.int.style = "ribbon",
      censor = F,
      ggtheme = theme_classic(),
      break.time.by = 365.25,
      font.y=c(15),
      font.x=c(15),
      font.tickslab=c(12,"bold"),
      font.legend=c(13, "bold"),
      font.title=c(17,"italic"),
      palette = "jco"
    )
  }
  
  print(graphic)
}
# KM_surv_curv(data)
  
# Now function to plot and test logistic regression:
Survival_test <- function(data = sample_data, months = 12, plot_graphic = T, output_simple_test = F){
  # first create the survival data:
  # good reference: https://www.emilyzabor.com/tutorials/survival_analysis_in_r_tutorial.html
  surv_data <- dplyr::select(data, date = monitoring_date, plant_ID, treatment, species, alive) %>% 
    group_by(plant_ID) %>% 
    mutate(life_span = as.numeric(max(date) - min(date)),
           died = as.numeric(any(alive == "no"))) %>% 
    summarize(planting_date = min(date), treatment = unique(treatment), 
              species = unique(species), life_span = unique(life_span), 
              died = unique(died), planting_date = unique(planting_date),
              last_date = max(date)) %>% 
    mutate(treatment = as.factor(treatment)) %>% 
    dplyr::select(species, treatment, life_span, died, planting_date, last_date)
  max_last_date <- max(surv_data$last_date)
  
  # First, set an interval of time in months:
  period = (months*30.4375)
  # Then remove all plants planted within the last period (that haven't had at least that amount of time to grow):
  # For everything else, we can ask: did it survive after growing for that period of time?
  sub_surv_data <- filter(surv_data, planting_date <= max_last_date - period) %>% 
    mutate(survived_period = as.numeric(life_span > period))
  
  # Run the basic model:
  mod1 <- glm(data=sub_surv_data, survived_period ~ treatment, family="binomial")
  if(output_simple_test) return(mod1)
  
  # Do a post-hoc pairwise test between these:
  comps <- glht(mod1, linfct = mcp(treatment = "Tukey")) # multcomp package
  post_hoc_results <- tidy(summary(comps))
  
  # trying to add letters to show significance but it's not working. Instead
  # for now will just export the summary result table
#   library(multcompView)
#   sum <- summary(comps)
#   p_vals <- as.numeric(sum$test$pvalues)
#   comp_names <- names(sum$test$tstat)
#   
#   names(comps)
#   sum$
#   
#   dif1 <- p_vals
#   names(dif1) <- comp_names
#   
#   multcompLetters(dif1)
#   
#   multcompLetters4(mod1, data=sub_surv_data)
#   # plot(comps)
#   row.names(comps$linfct)
#   multcompLetters(comps)
#   sum <- summary(comps)
#   sum$vcov
#   multcompLetters(sum)
#   multcompLetters(summary(comps))
#   
#   dif3 <- c(FALSE, FALSE, TRUE)
#   names(dif3) <- c("a-b", "a-c", "b-c")
#   multcompTs(dif3)
#   multcompLetters(dif3)
  
  if(plot_graphic){
    # Now calculate the probability of survival for each treatment:
    mean_probs <- predict(mod1, data.frame(treatment = levels(sub_surv_data$treatment)), type = "response")
    trt_names <- str_to_title(levels(sub_surv_data$treatment))
    
    # Bootstrap 95% CI around these survival probabilities:
    # good reference: https://www.statmethods.net/advstats/bootstrapping.html
    # Function to obtain predicted survival probabilities
    predicted_surv_prob <- function(data, indices) {
      d <- sub_surv_data[indices,] # allows boot to select sample
      model <- glm(data=d, survived_period ~ treatment, family="binomial")
      predicted <- predict(model, data.frame(treatment = levels(data$treatment)), type = "response")
      return(predicted)
    }
    
    # bootstrapping with 1000 replications
    results <- boot(data = sub_surv_data, statistic = predicted_surv_prob, R=1000)
    
    # loop through each coefficient:
    CI_lower <- NULL
    CI_upper <- NULL
    for(i in 1:length(mean_probs)){
      # get 95% confidence intervals
      res <- boot.ci(results, type="perc", index=i)$percent[c(4,5)]
      CI_lower[i] <- res[1]
      CI_upper[i] <- res[2]
    }
    final_results <- tibble(CI_lower, CI_upper, mean_probs, trt_names)
    
    graphic <- ggplot(final_results, aes(x=trt_names)) +
      geom_point(aes(y = mean_probs), size=2) +
      geom_errorbar(aes(ymin = CI_lower, ymax = CI_upper), size=1, width=0.2) +
      labs(x = "Treatments", y = "Predicted Survival (±95% CI)") +
      theme_restor +
      theme(axis.text.x = element_text(angle=45, hjust=1, face=2),
            axis.text.y = element_text(face=2, size=14),
            panel.grid.major.x = element_line(size=0.5)) +
      scale_y_continuous(expand = c(0, 0), limits = c(0,1), labels = scales::percent)
    
    print(graphic)
  } else {
    return(post_hoc_results)
  }
}

# Survival_test(data = surv_data, months = 12, plot_graphic = T)
# Survival_test(data = surv_data, months = 12, plot_graphic = F)
