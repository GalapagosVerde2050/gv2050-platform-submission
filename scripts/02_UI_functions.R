###############################################
##### FUNCTIONS MAKING THE USER INTERFACE #####
###############################################
# Here are functions for making the user interface better such as 
# CSS styles, etc.


### CSS Styles for the dashboard theme:
dashboard_theme <- tags$head(tags$style(HTML('
                              /* logo */
                              .skin-blue .main-header .logo {
                              background-color: #5C9100;
                              }
                              
                              /* logo when hovered */
                              .skin-blue .main-header .logo:hover {
                              background-color: #5C9100;
                              }
                              
                              /* navbar (rest of the header) */
                              .skin-blue .main-header .navbar {
                              background-color: #5C9100;
                              }        
                              ')))

### Footer
footer <- div(style = "padding: 15px; text-align: center; background-color: #5C9100; color: white;",
              tags$footer("Developed with",
                tags$span(style = "font-family: 'Source Code Pro', monospace; font-size: 25px;" ,"Shiny"),
                "from",
                img(src = "https://rstudio.com/wp-content/uploads/2018/10/RStudio-Logo-Flat.png", height = "30px"),
                ".",
                br(),
                "R version 4.0.3 (2020-10-10). Code on  ", tags$a(style="color:#a4f740;", href ="https://github.com/LukaNeg/gv2050-platform-submission", target="_blank", icon("github"),"GitHub.")),
              br(),
              "Authors: ",tags$a(style="color:#a4f740;", href ="https://github.com/annagaby", target="_blank", icon("github"),"Anna Calle"),
              " and ", tags$a(style="color:#a4f740;", href ="http://lukaneg.github.io", target="_blank","Luka Negoita")
              
              
)

### Function for setting the color styles:
GV2050_colors <- function(type){
  colors <-
    c(control = "#E5C494",
      hydrogel = "#35C5CB",
      cocoon = "#FFD92F",
      cocoon_hydrogel = "#FC8D63",
      waterboxx = "#A6D854",
      waterboxx_hydrogel = "#8DA0CB",
      growboxx = "#cd7fe3", ### Temporary, Luka should probably choose the color ###
      dead = "#CBC6AB",
      dark = "#2E2D2D",
      light = "#EEEFED",
      GVgreen = "#2EB458",
      GVgreen_light = "#7DC477",
      CDFblue = "#354AA0")
  
  return(colors[type])
  if(type=="all") return(colors)
}

### Function for transparent colors:
t_col <- function(color, percent = 50) {
  rgb.val <- col2rgb(color)
  t.col <- rgb(rgb.val[1], rgb.val[2], rgb.val[3], max = 255, alpha = (100-percent)*255/100)
  invisible(t.col)
}

theme_restor <- list(
  theme_classic() +
    theme(plot.margin = unit(c(.7,.7,.7,.7), "cm"),
          strip.background = element_rect(color = NA),
          axis.text.x = element_text(size = 11),
          axis.text.y = element_text(size = 11),
          axis.title.x = element_text(size = 15, margin=ggplot2::margin(t=0.5, unit="cm")),
          axis.title.y = element_text(size = 15, margin=ggplot2::margin(r=0.5, unit="cm")),
          legend.title = element_text(size = 15),
          legend.text = element_text(size = 13),
          plot.title = element_text(size = 16),
          axis.line=element_line(),
          panel.spacing = unit(1, "lines"))
)

