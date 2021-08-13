## 20210701
* Enable interactive tabs for all three models: SEIRD, SEIaImIsRD, SEIRDAges
* Currently copy-and-paste all sub-models into `global.R`, which will be replaced by a simple import after merging branches in `comomodels`

## 20210708
* Use dashboard for better layout and design.
* SEIRDAge: interactive selecting specific age_range group for visualization of simulation.

## 20210720
* SEIRDAge: interactive loading contact matrices from selecting paths; interactive selecting country from list; automatically show list of age groups from loaded contact matrices

## 20210812
* built conda environment yml for the package
* Default sliderInputs for transmission params are toggled. Allow showing each param by clicking on corresponding activeButtons in the "Model" diagram panel

## To do
* SEIRDAge: data missing for some countries in population.rda
* SEIRDAge: automatically identify how many contact matrices are loaded, get object names and process accordingly
* Deploy
* Move activeButtons for params onto the edges of the diagram
* (Ben) - interactive diagrams & slider (one param slider only activated when clicking on the diagram): click on a parameter -> slider appear -> This might make it particularly easy to understand what each parameter corresponds to. (It would also reduce the number of sliders which were active at a given point in time.)
* Add dependencies for a R shiny app package?
