# Notes for developers

## Brief intro to shiny
Shiny is an R package built on top of HTML to develop web apps directly from R. It is mainly structured with two basic components: 
* `ui` - user-interface, specify inputs, outputs and layouts
* `server` - functions to take inputs and calculate outputs.

In this app, the two components are written in two separate files: `R/ui.R` and `R/server.R`. The `app.R` contains the main commands that create a shiny app object from an explicit ui/server pair.

## The core: comomodels
The `R/global.R` file is the current sources of `comomodels`, which is copy-and-pasted from different branches of [comomodels](https://github.com/Como-DTC-Collaboration/como-models). Once we merge the branches and finalise the model, this file can be replaced by simply importing `library(comomodels)` in the beginning of `R/app.R`.

[The shiny rstudio main website](https://shiny.rstudio.com/) is a good resource to explore the utilizations of shiny.

TO BE CONTINUED...
