#########################################
#### This is a Shiny web application ####
#########################################
#### To customize colors:
#### https://community.rstudio.com/t/shinydashboard-custom-box-colors-to-match-brand/14147
#### https://stackoverflow.com/questions/31711307/how-to-change-color-in-shiny-dashboard
#### HTML Tags:
#### https://shiny.rstudio.com/articles/tag-glossary.html


library(shiny)
library(shinyWidgets) 
library(shinydashboard)
library(tidyverse)

# Read in data
sample_data <- read_csv("data/hypothetical_data.csv")

# Source scripts
source("scripts/00_functions.R", local = T)
source("scripts/02_UI_functions.R", local = T)
source("scripts/TAB_about.R", local = T) 
source("scripts/TAB_restor_progress.R", local = T)
source("scripts/TAB_summary.R", local = T)
source("scripts/TAB_plant_planner.R", local = T)
source("scripts/TAB_monit_planner.R", local = T)
source("scripts/ELEMENT_statistical_test.R", local = T)
source("scripts/ELEMENT_monitoring_calendar.R", local = T)
source("scripts/ELEMENT_experiment_designer.R")
source("scripts/ELEMENT_fieldwork_figure.R", local = T)
source("scripts/ELEMENT_restoration_target.R", local = T)

# Sys.Date()
update_lab = "Last data update: 2021-03-05 (beta v0.8)"  
version_lab = "(beta v0.9)"
updates_notes = ""


#####################
### User interface
ui <- tagList(dashboardPage(skin="blue",
                    title="RestoR",
                    
                    dashboardHeader(title=div(strong("RestoR")), 
                                    titleWidth=280),
                    
                    dashboardSidebar(width = 280,
                                     div(style="padding-top:10px;padding-left:10px;text-align:center;", img(src = "logo_restoR2.png", height = 180)),
                                     sidebarMenu(
                                       menuItem("About", tabName = "about", icon = icon("list-alt")),
                                       menuItem("Restoration Progress", tabName = "restor_progress", icon = icon("list-alt")),
                                       menuItem("Summarize and Analyze", tabName = "summary", icon = icon("map")),
                                       menuItem("Planting Planner", tabName = "plant_planner", icon = icon("table")),
                                       menuItem("Monitoring Planner", tabName = "monit_planner", icon = icon("table"))
                                     ),
                                     div(style="text-align:left; padding-left: 10px; padding-bottom: 15px; padding-top: 10px;",tags$h5(paste(update_lab, version_lab, sep="  "))),
                                     div(style="text-align:justify; padding-left: 10px; padding-right: 10px; padding-top: 30px;", 
                                         tags$hr(),
                                         "Improvements with the latest update:",
                                         tags$br(),
                                         version_lab,
                                         tags$br(),
                                         tags$i(updates_notes))
                    ),
                    
                    dashboardBody(
                      tabItems(
                        about_tab, ### About Us Tab
                        restor_prog_tab, ### Restoration Progress Tab
                        #data_entry_tab, ### Data Entry Tab
                        summary_tab, ### Summary Tab
                        plant_planner_tab, ### Planting Planner
                        monit_planner_tab ### Monitoring Planner
                    ),
                    
                      ### HTML/CSS tags
                      dashboard_theme
                    )
),

footer
)

########################
### Server (the backend)
server <- function(input, output, session) {
  res_mod <- callModule(
    module = selectizeGroupServer,
    id = "my-filters",
    data = sample_data,
    vars = c("species", "treatment", "site")
  )
  
  ### Restoration Progress Graph ###
  output$targetPlot <- renderPlot({
    req(input$targetInput)
    
    target_plot(dat_all = sample_data, target = input$targetInput)
  })
  
  total_alive <- sample_data %>% 
    group_by(plant_ID) %>% 
    filter( monitoring_date == max(monitoring_date)) %>% 
    filter(state != "dead") %>% 
    nrow()
  
  # Progress Info Box
  output$progressBox <- renderInfoBox({
    
    req(input$targetInput)
    
    if(input$targetInput > total_alive)
    infoBox(
      "Progress", paste0( total_alive*100/input$targetInput, "%"),
      "(Only live plants considered)",
      icon = icon("clock"),
      color = "yellow", fill = T
    ) else (
      infoBox(
        "Progress", paste0(total_alive*100/input$targetInput, "%"),
        "(Only live plants considered)",icon = icon("star"),
        color = "green", fill = T
      )
    )
  })
  
  #create the map
  output$leafletMap <- renderLeaflet({
    # data
    data_final <- filter(res_mod(), monitoring_date >= input$dateRange[1] & monitoring_date <= input$dateRange[2]) %>%
      group_by(plant_ID, treatment, species, lat, long) %>%
      summarise(date = min(planting_date))

    # color palette for circle markers
    pal <- colorFactor(c("#E5C494", "#35C5CB", "#FFD92F", "#FC8D63", "#A6D854", "#8DA0CB"), domain = data_final$treatment)

    # coordinates for setView
    mean_lat <- mean(data_final$lat)
    mean_long <- mean(data_final$long)

    # zoom level for setView
    zoom_level <- 14

    # map
    leaflet(data_final) %>%
      setView(lng = mean_long, lat = mean_lat, zoom = zoom_level)  %>%
      addTiles() %>%
      # radius is in meters:
      addCircles(data = data_final,
                 lat = ~ lat,
                 lng = ~ long,
                 weight = 1,
                 radius = 1,
                 label = ~as.character(paste0("Plant ID: ", sep = " ", plant_ID)),
                 color = ~pal(treatment),
                 fillOpacity = 0.5) %>%
      addLegend(pal = pal,
                values = data_final$treatment,
                opacity = 1,
                title = "Treatment",
                labFormat = labelFormat(
                  transform = function (x) str_to_title(sub("_", " ", x)))) %>%
      addMiniMap(toggleDisplay = TRUE,
                 mapOptions = leafletOptions(minZoom = 5, maxZoom = 12),
      ) %>%
      addScaleBar(position = "bottomleft") %>%
      addFullscreenControl() # adds full screen button
  })

  ### Monitoring Calendar ###
  output$monitoring_calendar <- renderPlotly({
    monitor_schedule(cleaned_data = sample_data)
  })

  ### Monitoring Spreadsheet ###
  # Create df
  datasheet_df <- reactive({
    sample_data %>%
    filter(site %in% input$selectSiteDatasheets) %>% 
    dplyr::select(plant_ID, treatment, site, lat, long) %>%
    mutate(monitoring_date = "") %>%
    mutate(state = "") %>% 
    mutate(observations = "") %>%  
    unique() %>% 
      rename("Plant ID" = plant_ID
             , "Treatment" = treatment,
             "Site" = site,
             "Lat" = lat,
             "Long" = long,
             "Monitoring Date" = monitoring_date,
             "State" = state,
             "Observations" = observations)
  })
  
  # Download button
  output$download_datasheet <- downloadHandler(
      filename = function() {
        paste("spreadsheet_", input$selectSiteDatasheets, ".csv", sep = "")
      },
      content = function(file) {
        write.csv(datasheet_df(), file, row.names = FALSE)
      }
  
    )

  ### Missed Plants ###
  output$missedUI <- renderUI({
    # UI elements to be rendered
    div(
      selectInput(
        "selectMissedSite",
        label = h4("Select site"),
        choices = unique(sample_data$site)
      ),
      dataTableOutput("missedDataTable"),
      downloadButton(("download_missed"), label = "Download")
    )
  })
  
  output$missedDataTable <- renderDataTable(
    extract_missed(sample_data)
  )
  
  ### GPX Download Button ###
  # Create file w/ coord
  coord_data <- sample_data %>% 
    dplyr::select(plant_ID, lat, long) %>% 
    unique()
  
  coords<-data.frame(ID=coord_data$plant_ID,Long= coord_data$long,
                     Lat=coord_data$lat)
  
  # Download GPX file
  output$download_GPX <- downloadHandler(
      filename = "plants_Baltra.gpx",
      content = function(file) {
        writeGPX(coords, file, type = "w")
      }
  )
  

  # Render UI for Yearly Overview Table
  output$yearsUI <- renderUI({
    # UI elements to be rendered
    div(
      selectInput(
        "selectYear",
        label = h4("Select year"),
        choices = sort(unique(year(sample_data$monitoring_date))),
        selected = max(year(sample_data$monitoring_date))
      )
    )
  })
  
  ### Fieldwork Waffle Chart
  # Render fieldwork waffle chart
  output$fieldworkPlot <- renderPlotly({
    
    req(input$selectYear)

    # Call function fieldwork_waffle()
    fieldwork_waffle(data = sample_data,
                     year = input$selectYear)
  })


  output$speciesBox <- renderValueBox({
    data_final <- filter(res_mod(), monitoring_date >= input$dateRange[1] & monitoring_date <= input$dateRange[2])
    unique_spp <- length(unique(data_final$species))
    valueBox(color="green", icon = icon("list"),
      div(style="vertical-align:middle; font-size:48px; display:inline-block",
          img(src = "plant_w.png", height = 50),
          strong(paste(unique_spp))
      ),
      subtitle=div(style="font-size:28px;",
                   strong("Species"))
    )
  })

  output$siteBox <- renderValueBox({
    data_final <- filter(res_mod(), monitoring_date >= input$dateRange[1] & monitoring_date <= input$dateRange[2])
    unique_isla <- length(unique(data_final$site))
    valueBox(color="green", icon = icon("list"),
             div(style="vertical-align:middle; font-size:48px; display:inline-block",
                 img(src = "map_marker_white.png", height = 50),
                 strong(paste(unique_isla))
             ),
             subtitle=div(style="font-size:28px;",
                          strong("Sites"))
    )
  })
  
  output$plantBox <- renderValueBox({
    data_final <- filter(res_mod(), monitoring_date >= input$dateRange[1] & monitoring_date <= input$dateRange[2])
    unique_plants <- length(unique(data_final$plant_ID))
    if(unique_plants>1000){
      value <- unique_plants/1000
      value <- round(value,1)
      unique_plants <- paste(value, "K", sep="")
    }
    valueBox(color="green", icon = icon("list"),
             div(style="vertical-align:middle; font-size:48px; display:inline-block",
                 img(src = "plant hand white.png", height = 50),
                 strong(paste(unique_plants))
             ),
             subtitle=div(style="font-size:28px;",
                          strong("Total Plantings"))
    )
  })
  
  
  #### PLANTING PLANNER TAB
  
  KM_data <- eventReactive(input$KM_plots, {
    data_final <- filter(res_mod(), monitoring_date >= input$dateRange[1] & monitoring_date <= input$dateRange[2])
  })
  
  output$KM_surv_plot <- renderPlot({
    KM_surv_curv(data = KM_data(), all_spp = F)
  })
  
  Log_data <- eventReactive(input$Surv_test, {
    data_final <- filter(res_mod(), monitoring_date >= input$dateRange[1] & monitoring_date <= input$dateRange[2])
  })
  
  output$Pred_surv_plot <- renderPlot({
    Survival_test(data = Log_data(), months = 12, plot_graphic = T)
  })
  
  
  output$experim_design_report <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = "experimental_design.html",
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "experiment_design_v2.Rmd")
      file.copy("experiment_design_v2.Rmd", tempReport, overwrite = TRUE)

      # Set up parameters to pass to Rmd document
      params <- list(name = input$design_name,
                     survival_data = sample_data,
                     site = input$site_type,
                     species = input$species_type,
                     spp_num = input$spp_numbers,
                     restor_prop=input$restor_prop)

      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    })
}
########

##########################
### Run the application 
shinyApp(ui = ui, server = server)
