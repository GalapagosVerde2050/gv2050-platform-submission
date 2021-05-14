###############################################
######## FIELD WORK DAYS WAFFLE CHART #########
###############################################

fieldwork_waffle <- function(data, year) {
  # Get unique dates of fieldwork days
  dates_data <- data %>% 
    dplyr::select(monitoring_date) %>% 
    unique()
  
  # Create tibble to add fieldwork and non fieldwork days
  fieldwork_tibble <- tibble(
    monitoring_date = seq(
      as.Date(paste0("01-01-", year), "%d-%m-%Y"),
      as.Date(paste0("31-12-", year), "%d-%m-%Y"),
      "days"
    ), 
    year = format(monitoring_date, "%Y"),
    week = as.integer(format(monitoring_date, "%W")) + 1, # Week starts at 1
    day = factor(weekdays(monitoring_date, T),
                 levels = rev(c(
                   "Mon", "Tue", "Wed", "Thu",
                   "Fri", "Sat", "Sun"
                 ))
    ),
    fieldwork = ifelse(monitoring_date %in% dates_data$monitoring_date, 'yes', 'no')
  )
  
  waffle_chart <- ggplot(fieldwork_tibble, aes(x = week, y = day, fill = fieldwork))  +
    geom_tile(color = "white", size = 0.7, aes(text= format(monitoring_date, "%b %d, %Y"))) +
    facet_wrap("year") +
    scale_x_continuous(
      expand = c(0, 0),
      breaks = seq(1, 52, length = 12),
      labels = c(
        "Jan", "Feb", "Mar", "Apr", "May", "Jun",
        "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"
      )
    )  +
    theme_linedraw(base_family = "Helvetica") +
    theme(
      axis.title = element_blank(),
      axis.ticks = element_blank(),
      axis.text.y = element_text(size = 7),
      panel.grid = element_blank(),
      legend.position = "bottom",
      legend.key.width = unit(1, "cm"),
      strip.text = element_text(hjust = 0.00, face = "bold", size = 12)
    ) +
    scale_fill_manual(values=c("paleturquoise2", "lightseagreen")) +
    coord_fixed(ratio = 1)
  
  waffle_chart <- ggplotly(waffle_chart,
                           tooltip = 'text',
                           width = 1100,
                           height = 268) %>%
    layout(legend = list(orientation = "h",  
                         y = -0.15,
                         title=list(text='<b> Fieldwork </b>')))
  
  return(waffle_chart)
}
