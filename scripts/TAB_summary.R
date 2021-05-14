###############################################
########## SUMMARIZE AND ANALYZE TAB ##########
###############################################
summary_tab <- tabItem(tabName = "summary",
                    div(style="text-align:left",tags$h2("Data Summary/Analysis")),
                    div(style="text-align:left",tags$h4("Tools for summarizing, visualizing, and learning from your results.")),
                    
                    fluidRow(
                      column(
                        width = 12, 
                        panel(
                          selectizeGroupUI(
                            id = "my-filters",
                            params = list(
                              species = list(inputId = "species", title = "Species:"),
                              treatment = list(inputId = "treatment", title = "Treatment:"),
                              site = list(inputId = "site", title = "Site:")
                            ),
                            dateRangeInput('dateRange',
                                           label = 'Date range: yyyy-mm-dd',
                                           start = "2010-05-12", end = Sys.Date()
                            )
                          )
                        )
                      )
                      ),
                      fluidRow(
                        column(width = 12,
                               #infoBox(width = 4, "hellow world", 50, fill=T),
                               #fancy_text_2(text_disp="Species", icon="plant.png", value=733),
                               #sum_box_style(color='#5CA860'),
                               fluidRow(
                                 column(width=4,uiOutput("speciesBox")),
                                 column(width=4,uiOutput("siteBox")),
                                 column(width=4,uiOutput("plantBox"))
                               )
                        )
                      ),
                      fluidRow(
                        column(width = 8,
                               div(style="text-align:left",tags$h3("Apply Statistical Analyses")),
                               
                               tabBox(
                                 # The id lets us use input$tabset1 on the server to find the current tab
                                 id = "tabset1", width = "90%",
                                 tabPanel("KM-Survival Curves",
                                          actionButton("KM_plots", "Analyze Kaplan-Meier Survival"),
                                          br(),
                                          "Note: Please wait after clicking. The analysis may take up to a minute to process.",
                                          br(),
                                          br(),
                                          fluidRow(
                                            plotOutput("KM_surv_plot",width="90%", height = 500)
                                          )
                                 ),
                                 
                                 tabPanel("Survival Test",
                                          actionButton("Surv_test", "Predict Survival by Treatment"),
                                          br(),
                                          "Note: Please wait after clicking. The analysis may take up to a minute to process.",
                                          br(),
                                          br(),
                                          fluidRow(
                                            plotOutput("Pred_surv_plot",width="90%", height = 500)
                                          )
                                 )
                               )
                        ),
                        column(width = 4,
                               div(style="text-align:left",tags$h3("What can we learn?")),
                               box(width = "100%", status = "success",
                                   "These statistical analyses help us learn what treatments work best 
                               for increasing the survival of plants after we plant them. They can also
                               be important to share with stakeholders and other 
                               scientists to show what works and what doesn't. The results from
                                   these analyses also go into the Planting Planner here, where you can generate
                                   new experimental designs that acount for what treatments work best."
                               ),
                               br(),
                               div(style="text-align:left",tags$h3("Meta Data")),
                               
                               box(width = "100%", status = "success",
                                   "The data presented as a sample in this app are only HYPOTHETICAL
                                   These data are NOT REAL. Unfortunately, but due to data sharing restrictions
                                   we are unable to share the original datasets from our GV2050 project."
                               )
                        
                        )
                      ),
                    fluidRow(
                      column(width = 8,
                             div(style="text-align:left",tags$h3("Map of plantings")),
                             
                               box(width = "100%", status = "success",
                                   leafletOutput(outputId = "leafletMap")  
                               )
                             
                             ),
                      column(width = 4
                             
                             
                            )
                    ),
                    #### Other summary stats
                    fluidRow(
                      column(width = 12,
                             div(style="text-align:left",tags$h3("Fieldwork Days")),
                             box(width = "100%", status = "success",
                                 # div(style="text-align:left",tags$h4("Past Expeditions")),
                                 # DT::dataTableOutput(outputId = "expeditionsTable", width = "180%"),
                                 # uiOutput("downloadPastUI"),
                                 # tags$br(),
                                 # div(style="text-align:left",tags$h4("Yearly Overview")),
                                 # uiOutput("yearsUI"),
                                 # tableOutput("yearlyTable"),
                                 # uiOutput("downloadYearlyUI"),
                                 # tags$br(),
                                 uiOutput("yearsUI"),
                                 plotlyOutput("fieldworkPlot"),
                                 tags$br()
                             )
                      )
                             
                      )
                    
                      # 
                      # 
                      # fluidRow(
                      #   column(width = 6,
                      #          plotOutput(outputId = "restorPlot", width="100%")),
                      #          #test table
                      #          #DT::dataTableOutput(outputId = "tabletest")),
                      #   
                      #   column(width = 6,
                      #          leafletOutput(outputId = "mymap", width = "100%"))
                      # ),
                      # fluidRow(
                      #   column(width = 6,
                      #          plotOutput(outputId = "survival", width="100%"))
                      # ),

                    )

