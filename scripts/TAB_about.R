###############################################
############### ABOUT TAB #####################
###############################################
about_tab <- tabItem(tabName = "about",
                     div(style="text-align:left",tags$h2("Welcome to RestoR!")),
                     fluidRow(
                       column(width = 6,
                              div(style="text-align:left",tags$h3("The Story of RestoR")),
                              box(width = "100%", status = "success",
                                  img(src = "CDF_GV2050_logo.jpg", width = "100%", style="display: block; margin-left: auto; margin-right: auto;"),
                                  
                                  p("RestoR was born from the need to make the process of restoring degraded
                                ecosystems of the Galapagos Islands more efficient. It all started when
                                members of the ",
                                    tags$a("Galapagos Verde 2050 (GV2050)", href = "http://www.galapagosverde2050.com", target = "_blank"),
                                    " restoration project at the ",
                                    tags$a("Charles Darwin Foundation", href = "https://www.darwinfoundation.org/en/", target = "_blank"),
                                    " began using a Shiny app to create accurate summary figures of their monitoring
                              data to share with stakeholders. As time went by, the team discovered new valuable
                              features that could be integrated into the app to reduce the time spent on tasks such
                              as analyzing monitoring data and planning for field trips. With the help from the app,
                              the GV2050 team has been able to spend more time in the field doing what matters most,
                              restoring the unique ecosystems of the Galapagos. Today, the GV2050 team uses a version
                              of RestoR to manage 90+ restoration sites with over 12,000 plants."),

                                  img(src = "mina granillo.jpg", width = "100%", style="display: block; margin-left: auto; margin-right: auto; max-width:100%;", ))
                       ),
                       column(width = 6,
                              div(style="text-align:left",tags$h3("Adaptive Management")),
                              box(width = "100%", status = "success",
                                  p("Adaptive management can be thought of as a cycle in which we iterate over the process of collecting data and learning from it, improving our methods, planning more fieldwork, and restoring more species! RestoR is the essential tool for any RestoRation project by providing some useful tools for organizing and managing each of these steps."),
                                  img(src="adaptive cycle_better.png",
                                      style = "max-width:100%;")),
                              div(style="text-align:left",tags$h3("What is RestoR?")),
                              box(width = "100%", status = "success",
                                  p("A tool to make ecological restoration through applied management more efficient. Users will encounter features to facilitate every step of the restoration process, from planting and monitoring, to evaluating the progress toward restoration targets. "),
                                  img(src="plant_2.png",
                                      style = "max-width:100%;"))
                       )
                     ),
                     fluidRow(
                       column(width = 6,
                       ),
                       column(width = 6,
                              div(style="text-align:left",tags$h3("How to use RestR?")),
                              box(width = "100%", status = "success",
                                  "RestR was designed to have an intuitive UI that follows the steps of the restoration process. RestoR allows you to:",
                                  tags$ul(
                                    tags$li("Keep track of the pogress towards your restoration target"),
                                    tags$li("Analyze/Summarize Data"),
                                    tags$li("Plan for planting expeditions"),
                                    tags$li("Plan for monitoring expeditions"),
                                    tags$b("We've included a hypothetical sample dataset for you to explore before choosing to adapt this app to your own project."),
                                  ))
                       )
                     )
)