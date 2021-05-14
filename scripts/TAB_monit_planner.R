###############################################
##### MONITORING PLANNER TAB #####
###############################################

monit_planner_tab <- tabItem(tabName = "monit_planner",
                                 div(style="text-align:left",tags$h2("Monitoring Planner")),
                             div(style="text-align:left",tags$h4("Tools for planning your monitoring fieldtrips.")),
                                 fluidRow(
                                   column(width = 12,
                                          div(style="text-align:left",tags$h3("Monitoring Calendar")),
                                          box(width = "100%", status = "success",
                                          plotlyOutput(outputId = "monitoring_calendar"))
                                          )),
                             fluidRow(
                               column(width = 6,
                                          div(style="text-align:left",tags$h3("Downloadable Monitoring Spreadsheet")),
                                      box(width = "100%", status = "success",
                                          selectInput(
                                                  "selectSiteDatasheets",
                                                  label = h4("Select site"),
                                                  choices = unique(sample_data$site)),
                                          downloadButton(("download_datasheet"), label = "Download"),
                                          )),
                               column(width = 6,
                                   div(style="text-align:left",tags$h3("GPX File With Plant Locations")),
                                   box(width = "100%", status = "success",
                                       div("Download a GPX file ready to be uploaded to your favorite GPS unit. This file contains the ID and location of all plants in all sites. In other words, no more lost plants!"),
                                       hr(),
                                       p("To visualize the GPX file without a GPS unit import it to ",
                                           a("Google Maps", href="https://www.google.com/maps/d/", target="_blank"),
                                           "."),
                                   downloadButton(("download_GPX"), label = "Download")
                                   )
                                 )
                             )
)
