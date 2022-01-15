# Notes for developers

## Brief intro to shiny
Shiny is an R package built on top of HTML to develop web apps directly from R. It is mainly structured with two basic components: 
* `ui` - user-interface, specify inputs, outputs and layouts
* `server` - functions to take inputs and calculate outputs.

In this app, the two components are written in two separate files: `R/ui.R` and `R/server.R`. The `app.R` contains the main commands that create a shiny app object from an explicit ui/server pair.

## The core: comomodels
`comomodels` is the core package for tha app which contains all submodels we need to demonstrate, data and supporting functions. In `app.R`, we install it from github by calling `devtools::install_github("Como-DTC-Collaboration/como-models")` and load the package.



## Useful resources for shiny development
* [The shiny rstudio main website](https://shiny.rstudio.com/)
* [Awesome R Shiny Resources & Extensions](https://paulvanderlaken.com/2021/02/16/awesome-r-shiny-resources-extensions/)

TO BE CONTINUED...
