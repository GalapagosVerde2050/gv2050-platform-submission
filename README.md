# RestoR
### Authors: Luka Negoita, Anna Calle

### Abstract: A tool to make ecological restoration through adaptive management more efficient. Users will encounter features to facilitate every step of the restoration process, from planting and monitoring, to evaluating the progress toward restoration targets.

### The Story Behind RestR
RestoR was born from the need to make the process of restoring degraded ecosystems of the Galapagos Islands more efficient. It all started when members of the Galapagos Verde 2050 (GV2050) restoration project at the Charles Darwin Foundation began using a Shiny app to create accurate summary figures of their monitoring data to share with stakeholders. As time went by, the team discovered new valuable features that could be integrated into the app to reduce the time spent on tasks such as analyzing monitoring data and planning for field trips. With the help from the app, the GV2050 team has been able to spend more time in the field doing what matters most, restoring the unique ecosystems of the Galapagos. Today, the GV2050 team uses a version of RestoR to manage 90+ restoration sites with over 12,000 plants.

### How to use RestR?
RestR was designed to have an intuitive UI that follows the steps of the restoration process. The user should expect to see the following tabs and features:

### 1. About
 - A page about us, our project, and the app!

### 2. Restoration Progress
 - Restoration Target: Allows the user to enter a target number of plants and visualize the progress towards that target with an area graph.
 - Progress to Target: Information box showing percentage of restoration target achieved. The appearance of the box changes based on if the target has been met or not.

### 3. Summarize and Analyze
 - Summary boxes: Boxes displaying summary values for species, sites, and total plantings.
 - Statistical Analyses
 - Map of Plantings: Leaflet map showing the location of all plants, and the experimental treatments applied to them
 - Fieldwork Days Chart: Interactive waffle chart showing field work days and non-field work days for the year chosen by the use

### 4. Planting Planner
 - Experimental Designer: Allows the user to generate a report with the experimental design needed for a planting based on user’s inputs. The experimental designed takes into consideration if the goal is closer to exclusively restoring with the best treatment or exclusively learning what is the best treatment

### 5. Monitoring Planner
 - Monitoring Calendar: Shows a bar graph with restoration sites and how long ago they were monitored. It uses a traffic light color palette to show which sites should be monitored first.
 - Downloadable Monitoring Spreadsheet: Downloads a csv. file with pre-filled columns that can be printed and used to collect monitoring data.
GPX File With Plant Locations: Downloads a GPX file that is ready to be uploaded to a GPS in case it is necessary to look up the location of a specific plant ID

Keywords: conservation, restoration, planning, data analysis, adaptive management, plotly, leaflet
Shiny app: RestoR 
Repo: GitHub - LukaNeg/gv2050-platform-submission
RStudio Cloud: https://rstudio.cloud/project/2554072
