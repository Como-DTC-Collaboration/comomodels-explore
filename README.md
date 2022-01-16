# comoexplore
This is a shiny app accompanied with the R package [`comomodels`](https://github.com/Como-DTC-Collaboration/como-models/) to interactively explore the model behaviors of in response to different transmission parameters.

Currently we support five sub-models: 

* **SEIRD** - basic Susceptible-Exposed-Infected-Recovered-Dead model;
* **SEIaImIsRD** - a symptom-compartment SEIRD model that divide the infectious population (I) into asymptomatic (Ia), mild (Im) and severe (Is) groups according to the severity of symptoms;
* **SEIRDAge** - an age-structured SEIRD model that separate the population into different age ranges.
* **SEIRDV** - an SEIRD model that includes the vaccinated population group.
* **SEIRD_RU** - an SEIRD model that considers two interacting communities: urban and rural.

## Development
To contribute to this repository, please read [DEVELOPMENT.md](https://github.com/Como-DTC-Collaboration/comomodels-explore/blob/main/DEVELOPMENT.md)


## How to run the app
### Run the deployed app online
Click [here](https://comodtc.shinyapps.io/comomodels-explore/) to easily explore `comomodels-explore` online.


### Run the app locally (linux)
In addition to the online version, `comomodels-explore` is also open accessible and can be run locally.
#### Dependencies
* Packages
    *   A number of R packages are required for the app. For the details please check [DESCRIPTION](https://github.com/Como-DTC-Collaboration/comomodels-explore/blob/main/DESCRIPTION) and the conda environment list [comomodels-explore.yml](https://github.com/Como-DTC-Collaboration/comomodels-explore/blob/main/envs/comomodels-explore.yml)
    *   If you have a number of dependencies not installed yet, instead of manually installing each of them, you can save time and energy by installing all dependencies in a `conda` environment:

        1. [Install conda](https://docs.conda.io/projects/conda/en/latest/user-guide/install/index.html)

        2. Create a package-specific conda environment:
            ```
            # clone the repository with git, go to the cloned directory:
            git clone git@github.com:Como-DTC-Collaboration/comomodels-explore.git
            cd comomodels-explore

            # create a new conda environment (here with name "comoexplore")
            # with all dependencies listed the provided .yml file:
            conda env create -f envs/comomodels-explore.yml

            # All dependenceis required by this package are now installed in comoexplore. Now activate the environment:
            conda activate comoexplore
            ```
    *   The core package `comomodels` need to be manually installed in R:
        ``` r
        devtools::install_github("Como-DTC-Collaboration/como-models")
        ```
* Data
    * Some models (e.g. SEIRDAge and SEIRD_RU) require the loading of extra data, such as social contact matrices between age groups and country-specific population data, etc. The default data in `comomodels` are in .rda format and also provided here in the `./data` folder, which are fully usable as an example. Referring to the same format, Users can also create and use customised data.

#### Installations
* **Method 1** - the easiest way is to directly download and run the remote version of the app in one command in R:

``` r
# 1. use runGitHub
shiny::runGitHub("comomodels-explore", "Como-DTC-Collaboration", ref = "main")

# 2. alternatively, run a tar or zip file
shiny::runUrl("https://github.com/Como-DTC-Collaboration/comomodels-explore/archive/main.tar.gz")
shiny::runUrl("https://github.com/Como-DTC-Collaboration/comomodels-explore/archive/main.zip")
```

* **Method 2** (recommended for developers) - you can first clone the git repository, then run the app:

```
git clone git@github.com:Como-DTC-Collaboration/comomodels-explore.git
cd comomodels-explore/

# start an R environment
R
```
In the R environment, make sure your working directory is `comomodels-explore` and all dependencies are installed. Then run the app

``` r
setwd(".")
## if you are using an interactive R session (e.g. RStudio):
shiny::runApp(appDir = ".")
## otherwise, set `launch.browser=FALSE` and manually launch the browser in the next step:
shiny::runApp(appDir = ".", launch.browser=FALSE)
```
Either way, a local URL will appear on your R console, e.g.:
``` r
Listening on http://127.0.0.1:5554
```

The corresponding interface may automatically pop out. If not, you can manually copy-and-paste the above URL to any web server. 

You can now play around with the parameters on the interface.
