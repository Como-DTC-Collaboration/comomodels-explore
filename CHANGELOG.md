## 20210701
* Enable interactive tabs for all three models: SEIRD, SEIaImIsRD, SEIRDAges
* Currently copy-and-paste all sub-models into `global.R`, which will be replaced by a simple import after merging branches in `comomodels`

## 20210708
* Use dashboard for better layout and design.
* SEIRDAge: interactive selecting specific `age_range` group for visualization of simulation.

## 20210720
* SEIRDAge: interactive loading contact matrices from selecting paths; interactive selecting country from list; automatically show list of age groups from loaded contact matrices

## 20210812
* Built conda environment yml for the package
* Default sliderInputs for transmission params are toggled. Display each param sliderInput bar only clicking on corresponding activeButtons in the "Model" diagram panel

## 20210813
* Display each param sliderInput bar only when clicking on corresponding nodes ub the "Model" diagram panel (Ben's suggestion)

## 20210820
* Add description for each population group or parameter (node) on the model diagram when clicking on.
* Add a separate plot for Incidence and Daily deaths.
* Aesthetics restructured (two cols in main panel)

## 20211012-20211013
* Add SEIRDV
* Add SEIRD_RU
* Aesthetics restructured (color_breweri; not using textsize=20)

## 202111
* Deployed at `shinyapps.io`

## 20220115
* Updated all current models (SEIRD, SEIaImIsRD, SEIRDAge, SEIRDV, SEIRD_RU) with latest [`comomodels`](https://github.com/Como-DTC-Collaboration/como-models/) main branch
* Replace actual packages codes by comomodels github packages installation
* Set default transmission params according to those in comomodels function `covid_transmission_parameters()` in SEIRD, SEIRD_RU, part of SEIRDAge & SEIRDV
 


## To do
* Check which default covid params can be applied to which model(s)
* New submodels to include: SEIRDVAge, SEIRDNPIAge, SEIRD_BD, SEIRD_CT
* Interactive modification: simulation time (start, end, interval)
* Interactive modification: contact matrices (age & time dependent)
* [SEIRDAge] [SEIRD_RU] diagram
* Add a brief description on how to read the SEIRD diagram.
* [SEIRDAge] data missing for some countries in population.rda
* [SEIRDAge] automatically identify how many contact matrices are loaded, get object names and process accordingly
* [SEIRDV] Add to slidebar: parameters of interventions
* [SEIRD_RU] Add plots with population summed across the R and Y communities

## Problems & thoughts
* [SEIRD_Age, SEIRD_RU] data/{population,percentrural_by_country,agedemographics_by_country,country.codetoname}.rda; consistent with the contact matrices?
* [SEIRD_RU] For some countries, the `contact_all_urban[[country]]` contains NAs -> error reading contact matrices
* [SEIRD_RU] Diagram showing showing 1-2-1 connections within/between communities, with strength (line width?) of connection depending on Connectedness param?

