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
* Default sliderInputs for transmission params are toggled. Display each param sliderInput bar only clicking on corresponding activeButtons in the "Model" diagram panel

## 20210813
* Display each param sliderInput bar only when clicking on corresponding nodes ub the "Model" diagram panel (Ben's suggestion)

## 20210820
* Add description for each population group or parameter (node) on the model diagram when clicking on.
* Add a separate plot for Incidence and Daily deaths.
* Aesthetics restructured (two cols in main panel)

## To do
* Add a brief description on how to read the SEIRD diagram.
* SEIRDAge: data missing for some countries in population.rda
* SEIRDAge: automatically identify how many contact matrices are loaded, get object names and process accordingly
* Deploy
* Docker environment?
