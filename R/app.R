# The main commands to create a shiny app object from
# an explicit ui/server pair. You can run the application by
# clicking 'Run App' button above, or run shiny::runApp("R")
# at the package directory.

# packages required by como-models
library(tidyverse) # tidyselect >1.1.1
library(deSolve)
library(ggplot2)
library(reshape2)
library(magrittr)

# packages for drawing model diagram
library(DiagrammeR)
# packages for building interactive plots
library(plotly)

# packages for building shiny app
library(shiny)
library(miniUI)
library(shinydashboard)
library(shinyjs)


# the core model (comomodels)
# - should be load before ui.R and server.R
# - after comomodels is finished we can load the model package directly
# - by library(comomodels)
source("R/global.R")

# ui and server
source("R/ui.R")
source("R/server.R")

shinyApp(ui, server)
