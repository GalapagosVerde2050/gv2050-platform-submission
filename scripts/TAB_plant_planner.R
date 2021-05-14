###############################################
########## PLANTING PLANNER TAB ###############
###############################################

plant_planner_tab <- tabItem(tabName = "plant_planner",
                             div(style="text-align:left",tags$h2("Planting Planner")),
                             div(style="text-align:left",tags$h4("Tools for planning your planting fieldtrips.")),
                             
                             
                             fluidRow(
                               column(width = 6,
                                 div(style="text-align:left",tags$h3("Input available seedlings and treatments:")),
                                      
                                 box(width = "100%", status = "success",
                                     textInput(inputId="design_name", "Design Name"),
                                     selectInput(inputId="site_type", "Site", choices=unique(sample_data$site),
                                                 multiple=F),
                                     selectInput(inputId="species_type", "Species", choices=unique(sample_data$species),
                                                 multiple=F),
                                     textInput(inputId="spp_numbers", "Number of seedlings available")
                                  
                                 )
                               ),
                               column(width = 6,
                                      div(style="text-align:left",tags$h3("Experiment vs Restore")),
                                      
                                      box(width = "100%", status = "success",
                                          "This slider allows you to select how much you want your next planting design to
                                          focus on planting using the treatments that work best
                                          (through the statistical analyses on the Summary/Analysis tab), versus planting in a
                                          more balanced design that helps you learn more about all possible treatments.",
                                          br(),
                                          "Usually the best approach is to begin at zero, where all planting designs are balanced
                                          and made to help you learn as much as you can. Then over time as you accumulate data and
                                          begin to learn more about what works, you can move the slider towards 1, which means more 
                                          plants will be used only with the treatment that works best.",
                                          br(),
                                          br(),
                                          sliderInput(inputId="restor_prop", label="Set degree of restoration:", 
                                                      min=0, max=1, value=0.5, step=0.1),
                                          
                                      ),
                                      div(style="text-align:left",tags$h3("Generate design")),
                                      box(width = "100%", status = "success",
                                          "Click here to generate a planting design.",
                                          br(),
                                          br(),
                                          downloadButton("experim_design_report", "Generate Planting Design")
                                          
                                      )
                                      
                               )
                             )

                             )


