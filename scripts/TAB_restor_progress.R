###############################################
######### RESTORATION PROGRESS TAB ############
###############################################

restor_prog_tab <- tabItem(tabName = "restor_progress",
                       div(style="text-align:left",tags$h2("Restoration Progress")),
                       div(style="text-align:left",tags$h4("Tools for visualizing your restoration progress.")),
                       
                       fluidRow(
                         column(
                           width = 8,
                           div(style="text-align:left",tags$h3("Restoration Target")),
                           box(width = "100%", status = "success",
                               numericInput("targetInput", label = h4("Enter target number of plants"), value = 1000),
                               plotOutput(outputId = "targetPlot", width="100%")
                                          
                           )
), 
column(
  width = 4,
  div(style="text-align:left",tags$h3("Why Have a Target?")),
  box(width = "100%", status = "success",
      "Ecological restoration requires identifying restoration targets that are informed by reference ecosystems. Restoration projects/programs can use these targets to plan and communicate a shared vision of their goals and to determine when restoration is achieved."
      
  ),
  div(style="text-align:left",tags$h3("Progress to target")),
  infoBoxOutput("progressBox", width = "100%")
)
)
)

