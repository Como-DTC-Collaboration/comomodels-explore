## 20210701
* Enable interactive tabs for all three models: SEIRD, SEIaImIsRD, SEIRDAges
* Currently copy-and-paste all sub-models into `global.R`, which will be replaced by a simple import after merging branches in `comomodels`

## 20210708
* Use dashboard for better layout and design.
* SEIRDAge: interactive selecting specific age_range group for visualization of simulation.

## 20210720
* SEIRDAge: interactive loading contact matrices from selecting paths; interactive selecting country from list; automatically show list of age groups from loaded contact matrices

## To do
* SEIRDAge: data missing for some countries in population.rda
* SEIRDAge: automatically identify how many contact matrices are loaded, get object names and process accordingly
* deploy
